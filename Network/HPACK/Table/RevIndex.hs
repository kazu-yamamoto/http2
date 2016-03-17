{-# LANGUAGE BangPatterns, OverloadedStrings, CPP, RecordWildCards #-}

module Network.HPACK.Table.RevIndex (
    RevIndex
  , newRevIndex
  , renewRevIndex
  , lookupRevIndex
  , insertRevIndex
  , deleteRevIndexList
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>), (<*>))
#endif
import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.Function (on)
import Data.CaseInsensitive (foldedCase)
import Data.IORef
import Data.List (groupBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Network.HPACK.Table.Entry
import Network.HPACK.Table.Static
import Network.HPACK.Token
import Network.HPACK.Types

----------------------------------------------------------------

data RevIndex = RevIndex !DynamicRevIndex !OtherRevIdex

type DynamicRevIndex = Array Int (IORef ValueMap)

data KeyValue = KeyValue HeaderName HeaderValue deriving (Eq, Ord)

-- We always create an index for a pair of an unknown header and its value
-- in Linear{H}.
type OtherRevIdex = IORef (Map KeyValue HIndex)

{-# SPECIALIZE INLINE M.lookup :: KeyValue -> M.Map KeyValue HIndex -> Maybe HIndex #-}
{-# SPECIALIZE INLINE M.delete :: KeyValue -> M.Map KeyValue HIndex -> M.Map KeyValue HIndex #-}
{-# SPECIALIZE INLINE M.insert :: KeyValue -> HIndex -> M.Map KeyValue HIndex -> M.Map KeyValue HIndex #-}

----------------------------------------------------------------

type StaticRevIndex = Array Int StaticEntry

data StaticEntry = StaticEntry !HIndex !(Maybe ValueMap)

type ValueMap = Map HeaderValue HIndex

{-# SPECIALIZE INLINE M.lookup :: HeaderValue -> M.Map HeaderValue HIndex -> Maybe HIndex #-}
{-# SPECIALIZE INLINE M.delete :: HeaderValue -> M.Map HeaderValue HIndex -> M.Map HeaderValue HIndex #-}
{-# SPECIALIZE INLINE M.insert :: HeaderValue -> HIndex -> M.Map HeaderValue HIndex -> M.Map HeaderValue HIndex #-}

----------------------------------------------------------------

staticRevIndex :: StaticRevIndex
staticRevIndex = A.array (minToken,maxToken) $ map toEnt zs
  where
    toEnt (k, xs) = (toIx (toToken k), m)
      where
        m = case xs of
            []  -> error "staticRevIndex"
            [(_,i)] -> StaticEntry i Nothing
            (_,i):_ -> let !vs = M.fromList xs
                       in StaticEntry i (Just vs)
    zs = map extract $ groupBy ((==) `on` fst) lst
      where
        lst = zipWith (\(k,v) i -> (k,(v,i))) staticTableList $ map SIndex [1..]
        extract xs = (fst (head xs), map snd xs)

{-# INLINE lookupStaticRevIndex #-}
lookupStaticRevIndex :: Int -> (HIndex -> IO ()) -> IO ()
lookupStaticRevIndex ix fd' = case staticRevIndex ! ix of
    StaticEntry i _ -> fd' i

----------------------------------------------------------------

newDynamicRevIndex :: IO DynamicRevIndex
newDynamicRevIndex = A.listArray (minToken,maxToken) <$> mapM mk lst
  where
    mk _ = newIORef M.empty
    lst = [minToken..maxToken]

renewDynamicRevIndex :: DynamicRevIndex -> IO ()
renewDynamicRevIndex drev = mapM_ clear [minToken..maxToken]
  where
    clear t = writeIORef (drev ! t) M.empty

{-# INLINE lookupDynamicStaticRevIndex #-}
lookupDynamicStaticRevIndex :: Int -> HeaderValue -> DynamicRevIndex
                            -> (HIndex -> IO ())
                            -> (HIndex -> IO ())
                            -> IO ()
lookupDynamicStaticRevIndex ix v drev fa' fbd' = do
    let ref = drev ! ix
    m <- readIORef ref
    case M.lookup v m of
        Just i  -> fa' i
        Nothing -> case staticRevIndex ! ix of
            StaticEntry i Nothing  -> fbd' i
            StaticEntry i (Just m') -> case M.lookup v m' of
                Nothing -> fbd' i
                Just j  -> fa' j

{-# INLINE insertDynamicRevIndex #-}
insertDynamicRevIndex :: Token -> HeaderValue -> HIndex -> DynamicRevIndex -> IO ()
insertDynamicRevIndex t v i drev = modifyIORef ref $ M.insert v i
  where
    ref = drev ! toIx t

{-# INLINE deleteDynamicRevIndex #-}
deleteDynamicRevIndex :: Token -> HeaderValue -> DynamicRevIndex -> IO ()
deleteDynamicRevIndex t v drev = modifyIORef ref $ M.delete v
  where
    ref = drev ! toIx t

----------------------------------------------------------------

newOtherRevIndex :: IO OtherRevIdex
newOtherRevIndex = newIORef M.empty

renewOtherRevIndex :: OtherRevIdex -> IO ()
renewOtherRevIndex ref = writeIORef ref M.empty

{-# INLINE lookupOtherRevIndex #-}
lookupOtherRevIndex :: Header -> OtherRevIdex -> (HIndex -> IO ()) -> IO () -> IO ()
lookupOtherRevIndex (k,v) ref fa' fc' = do
      oth <- readIORef ref
      case M.lookup (KeyValue k v) oth of
          Just i  -> fa' i
          Nothing -> fc'

{-# INLINE insertOtherRevIndex #-}
insertOtherRevIndex :: Token -> HeaderValue -> HIndex -> OtherRevIdex -> IO ()
insertOtherRevIndex t v i ref = modifyIORef' ref $ M.insert (KeyValue k v) i
  where
    !k = tokenFoldedKey t

{-# INLINE deleteOtherRevIndex #-}
deleteOtherRevIndex :: Token -> HeaderValue -> OtherRevIdex -> IO ()
deleteOtherRevIndex t v ref = modifyIORef' ref $ M.delete (KeyValue k v)
  where
    !k = tokenFoldedKey t

----------------------------------------------------------------

newRevIndex :: IO RevIndex
newRevIndex = RevIndex <$> newDynamicRevIndex <*> newOtherRevIndex

renewRevIndex :: RevIndex -> IO ()
renewRevIndex (RevIndex dyn oth) = do
    renewDynamicRevIndex dyn
    renewOtherRevIndex oth

{-# INLINE lookupRevIndex #-}
lookupRevIndex :: Token
               -> HeaderValue
               -> (HIndex -> IO ())
               -> (HeaderValue -> Entry -> HIndex -> IO ())
               -> (HeaderName -> HeaderValue -> Entry -> IO ())
               -> (HeaderValue -> Entry -> HIndex -> IO ())
               -> RevIndex
               -> IO ()
lookupRevIndex t@Token{..} v fa fb fc fd (RevIndex dyn oth)
  | isIxNonStatic ix = lookupOtherRevIndex (k,v) oth fa' fc'
  | shouldBeIndexed  = lookupDynamicStaticRevIndex ix v dyn fa' fb'
  | otherwise        = lookupStaticRevIndex ix fd'
  where
    k = foldedCase tokenKey
    ent = toEntryToken t v
    fa' = fa
    fb' = fb v ent
    fc' = fc k v ent
    fd' = fd v ent

----------------------------------------------------------------

{-# INLINE insertRevIndex #-}
insertRevIndex :: Entry -> HIndex -> RevIndex -> IO ()
insertRevIndex (Entry _ t v) i (RevIndex dyn oth)
  | isTokenNonStatic t = insertOtherRevIndex   t v i oth
  | otherwise          = insertDynamicRevIndex t v i dyn

{-# INLINE deleteRevIndex #-}
deleteRevIndex :: RevIndex -> Entry -> IO ()
deleteRevIndex (RevIndex dyn oth) (Entry _ t v)
  | isTokenNonStatic t = deleteOtherRevIndex   t v oth
  | otherwise          = deleteDynamicRevIndex t v dyn

{-# INLINE deleteRevIndexList #-}
deleteRevIndexList :: [Entry] -> RevIndex -> IO ()
deleteRevIndexList es rev = mapM_ (deleteRevIndex rev) es
