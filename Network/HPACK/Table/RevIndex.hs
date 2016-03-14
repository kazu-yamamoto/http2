{-# LANGUAGE BangPatterns, OverloadedStrings, CPP #-}

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

-- We always create an index for a pair of an unknown header and its value
-- in Linear{H}.
type OtherRevIdex = IORef (Map (HeaderName,HeaderValue) HIndex)

----------------------------------------------------------------

type StaticRevIndex = Array Int StaticEntry

data StaticEntry = StaticEntry !HIndex !(Maybe ValueMap)

type ValueMap = Map HeaderValue HIndex

----------------------------------------------------------------

staticRevIndex :: StaticRevIndex
staticRevIndex = A.array (minToken,maxToken) $ map toEnt zs
  where
    toEnt (k,xs) = (toIx (toToken k), m)
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

{-# INLINE deleteDynamicRevIndex#-}
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
lookupOtherRevIndex h ref fa' fc' = do
      oth <- readIORef ref
      case M.lookup h oth of
          Just i  -> fa' i
          Nothing -> fc'

{-# INLINE insertOtherRevIndex #-}
insertOtherRevIndex :: HeaderName -> HeaderValue -> HIndex -> OtherRevIdex -> IO ()
insertOtherRevIndex k v i ref = modifyIORef' ref $ M.insert (k,v) i

{-# INLINE deleteOtherRevIndex #-}
deleteOtherRevIndex :: HeaderName -> HeaderValue -> OtherRevIdex -> IO ()
deleteOtherRevIndex k v ref = modifyIORef' ref $ M.delete (k,v)

----------------------------------------------------------------

newRevIndex :: IO RevIndex
newRevIndex = RevIndex <$> newDynamicRevIndex <*> newOtherRevIndex

renewRevIndex :: RevIndex -> IO ()
renewRevIndex (RevIndex dyn oth) = do
    renewDynamicRevIndex dyn
    renewOtherRevIndex oth

{-# INLINE lookupRevIndex #-}
lookupRevIndex :: Header
               -> (HIndex -> IO ())
               -> (HeaderValue -> Entry -> HIndex -> IO ())
               -> (HeaderName -> HeaderValue -> Entry -> IO ())
               -> (HeaderValue -> Entry -> HIndex -> IO ())
               -> RevIndex
               -> IO ()
lookupRevIndex h@(k,v) fa fb fc fd (RevIndex dyn oth)
  | ix == otherToken = lookupOtherRevIndex h oth fa' fc'
  | should           = lookupDynamicStaticRevIndex ix v dyn fa' fb'
  | otherwise        = lookupStaticRevIndex ix fd'
  where
    t@(Token ix should _) = toToken k
    ent = toEntryToken h t
    fa' = fa
    fb' = fb v ent
    fc' = fc k v ent
    fd' = fd v ent

----------------------------------------------------------------

{-# INLINE insertRevIndex #-}
insertRevIndex :: Entry -> HIndex -> RevIndex -> IO ()
insertRevIndex (Entry _ t (k,v)) i (RevIndex dyn oth)
  | isTokenOther t = insertOtherRevIndex k v i oth
  | otherwise      = insertDynamicRevIndex t v i dyn

{-# INLINE deleteRevIndex #-}
deleteRevIndex :: RevIndex -> Entry -> IO ()
deleteRevIndex (RevIndex dyn oth) (Entry _ t (k,v))
  | isTokenOther t = deleteOtherRevIndex k v oth
  | otherwise      = deleteDynamicRevIndex t v dyn

{-# INLINE deleteRevIndexList #-}
deleteRevIndexList :: [Entry] -> RevIndex -> IO ()
deleteRevIndexList es rev = mapM_ (deleteRevIndex rev) es
