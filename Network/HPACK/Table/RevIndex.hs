{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HPACK.Table.RevIndex (
    RevIndex,
    newRevIndex,
    renewRevIndex,
    lookupRevIndex,
    lookupRevIndex',
    insertRevIndex,
    deleteRevIndexList,
) where

import Data.Array (Array)
import qualified Data.Array as A
import Data.Array.Base (unsafeAt)
import Data.Function (on)
import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Network.HTTP.Semantics

import Imports
import Network.HPACK.Table.Entry
import Network.HPACK.Table.Static
import Network.HPACK.Types

----------------------------------------------------------------

data RevIndex = RevIndex DynamicRevIndex OtherRevIdex

type DynamicRevIndex = Array Int (IORef ValueMap)

data KeyValue = KeyValue FieldName FieldValue deriving (Eq, Ord)

-- We always create an index for a pair of an unknown header and its value
-- in Linear{H}.
type OtherRevIdex = IORef (Map KeyValue HIndex)

{-# SPECIALIZE INLINE M.lookup ::
    KeyValue -> M.Map KeyValue HIndex -> Maybe HIndex
    #-}
{-# SPECIALIZE INLINE M.delete ::
    KeyValue -> M.Map KeyValue HIndex -> M.Map KeyValue HIndex
    #-}
{-# SPECIALIZE INLINE M.insert ::
    KeyValue -> HIndex -> M.Map KeyValue HIndex -> M.Map KeyValue HIndex
    #-}

----------------------------------------------------------------

type StaticRevIndex = Array Int StaticEntry

data StaticEntry = StaticEntry HIndex (Maybe ValueMap) deriving (Show)

type ValueMap = Map FieldValue HIndex

----------------------------------------------------------------

staticRevIndex :: StaticRevIndex
staticRevIndex = A.array (minTokenIx, maxStaticTokenIx) $ map toEnt zs
  where
    toEnt (k, xs) = (tokenIx $ toToken $ foldedCase k, m)
      where
        m = case xs of
            ("", i) :| [] -> StaticEntry i Nothing
            (_, i) :| _ ->
                let vs = M.fromList $ NE.toList xs
                 in StaticEntry i (Just vs)
    zs = map extract $ NE.groupBy ((==) `on` fst) lst
      where
        lst = zipWith (\(k, v) i -> (k, (v, i))) staticTableList $ map SIndex [1 ..]

        extract xs = (fst (NE.head xs), NE.map snd xs)

{-# INLINE lookupStaticRevIndex #-}
lookupStaticRevIndex
    :: Int -> FieldValue -> (HIndex -> IO ()) -> (HIndex -> IO ()) -> IO ()
lookupStaticRevIndex ix v fa' fbd' = case staticRevIndex `unsafeAt` ix of
    StaticEntry i Nothing -> fbd' i
    StaticEntry i (Just m) -> case M.lookup v m of
        Nothing -> fbd' i
        Just j -> fa' j

----------------------------------------------------------------

newDynamicRevIndex :: IO DynamicRevIndex
newDynamicRevIndex = A.listArray (minTokenIx, maxStaticTokenIx) <$> mapM mk' lst
  where
    mk' _ = newIORef M.empty
    lst = [minTokenIx .. maxStaticTokenIx]

renewDynamicRevIndex :: DynamicRevIndex -> IO ()
renewDynamicRevIndex drev = mapM_ clear [minTokenIx .. maxStaticTokenIx]
  where
    clear t = writeIORef (drev `unsafeAt` t) M.empty

{-# INLINE lookupDynamicStaticRevIndex #-}
lookupDynamicStaticRevIndex
    :: Int
    -> FieldValue
    -> DynamicRevIndex
    -> (HIndex -> IO ())
    -> (HIndex -> IO ())
    -> IO ()
lookupDynamicStaticRevIndex ix v drev fa' fbd' = do
    let ref = drev `unsafeAt` ix
    m <- readIORef ref
    case M.lookup v m of
        Just i -> fa' i
        Nothing -> lookupStaticRevIndex ix v fa' fbd'

{-# INLINE insertDynamicRevIndex #-}
insertDynamicRevIndex
    :: Token -> FieldValue -> HIndex -> DynamicRevIndex -> IO ()
insertDynamicRevIndex t v i drev = modifyIORef ref $ M.insert v i
  where
    ref = drev `unsafeAt` tokenIx t

{-# INLINE deleteDynamicRevIndex #-}
deleteDynamicRevIndex :: Token -> FieldValue -> DynamicRevIndex -> IO ()
deleteDynamicRevIndex t v drev = modifyIORef ref $ M.delete v
  where
    ref = drev `unsafeAt` tokenIx t

----------------------------------------------------------------

newOtherRevIndex :: IO OtherRevIdex
newOtherRevIndex = newIORef M.empty

renewOtherRevIndex :: OtherRevIdex -> IO ()
renewOtherRevIndex ref = writeIORef ref M.empty

{-# INLINE lookupOtherRevIndex #-}
lookupOtherRevIndex
    :: (FieldName, FieldValue) -> OtherRevIdex -> (HIndex -> IO ()) -> IO () -> IO ()
lookupOtherRevIndex (k, v) ref fa' fc' = do
    oth <- readIORef ref
    maybe fc' fa' $ M.lookup (KeyValue k v) oth

{-# INLINE insertOtherRevIndex #-}
insertOtherRevIndex :: Token -> FieldValue -> HIndex -> OtherRevIdex -> IO ()
insertOtherRevIndex t v i ref = modifyIORef' ref $ M.insert (KeyValue k v) i
  where
    k = tokenFoldedKey t

{-# INLINE deleteOtherRevIndex #-}
deleteOtherRevIndex :: Token -> FieldValue -> OtherRevIdex -> IO ()
deleteOtherRevIndex t v ref = modifyIORef' ref $ M.delete (KeyValue k v)
  where
    k = tokenFoldedKey t

----------------------------------------------------------------

newRevIndex :: IO RevIndex
newRevIndex = RevIndex <$> newDynamicRevIndex <*> newOtherRevIndex

renewRevIndex :: RevIndex -> IO ()
renewRevIndex (RevIndex dyn oth) = do
    renewDynamicRevIndex dyn
    renewOtherRevIndex oth

{-# INLINE lookupRevIndex #-}
lookupRevIndex
    :: Token
    -> FieldValue
    -> (HIndex -> IO ())
    -> (FieldValue -> Entry -> HIndex -> IO ())
    -> (FieldName -> FieldValue -> Entry -> IO ())
    -> (FieldValue -> HIndex -> IO ())
    -> RevIndex
    -> IO ()
lookupRevIndex t@Token{..} v fa fb fc fd (RevIndex dyn oth)
    | not (isStaticTokenIx tokenIx) = lookupOtherRevIndex (k, v) oth fa' fc'
    | shouldBeIndexed =
        lookupDynamicStaticRevIndex tokenIx v dyn fa' fb'
    -- path: is not indexed but ":path /" should be used, sigh.
    | otherwise = lookupStaticRevIndex tokenIx v fa' fd'
  where
    k = foldedCase tokenKey
    ent = toEntryToken t v
    fa' = fa
    fb' = fb v ent
    fc' = fc k v ent
    fd' = fd v

{-# INLINE lookupRevIndex' #-}
lookupRevIndex'
    :: Token
    -> FieldValue
    -> (HIndex -> IO ())
    -> (FieldValue -> HIndex -> IO ())
    -> (FieldName -> FieldValue -> IO ())
    -> IO ()
lookupRevIndex' Token{..} v fa fd fe
    | isStaticTokenIx tokenIx = lookupStaticRevIndex tokenIx v fa' fd'
    | otherwise = fe'
  where
    k = foldedCase tokenKey
    fa' = fa
    fd' = fd v
    fe' = fe k v

----------------------------------------------------------------

{-# INLINE insertRevIndex #-}
insertRevIndex :: Entry -> HIndex -> RevIndex -> IO ()
insertRevIndex (Entry _ t v) i (RevIndex dyn oth)
    | isStaticToken t = insertDynamicRevIndex t v i dyn
    | otherwise = insertOtherRevIndex t v i oth

{-# INLINE deleteRevIndex #-}
deleteRevIndex :: RevIndex -> Entry -> IO ()
deleteRevIndex (RevIndex dyn oth) (Entry _ t v)
    | isStaticToken t = deleteDynamicRevIndex t v dyn
    | otherwise = deleteOtherRevIndex t v oth

{-# INLINE deleteRevIndexList #-}
deleteRevIndexList :: [Entry] -> RevIndex -> IO ()
deleteRevIndexList es rev = mapM_ (deleteRevIndex rev) es
