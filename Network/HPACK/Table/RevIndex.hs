{-# LANGUAGE BangPatterns #-}

module Network.HPACK.Table.RevIndex (
    RevIndex
  , RevResult(..)
  , newRevIndex
  , renewRevIndex
  , lookupRevIndex
  , insertRevIndex
  , deleteRevIndexList
  ) where

import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.Function (on)
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Network.HPACK.Table.Static
import Network.HPACK.Table.Token
import Network.HPACK.Types

----------------------------------------------------------------

data RevResult = N | K HIndex | KV HIndex

----------------------------------------------------------------

data RevIndex = RevIndex DynamicRevIndex OtherRevIdex

type DynamicRevIndex = Array Token (IORef ValueMap)

type OtherRevIdex = IORef (Map (HeaderName,HeaderValue) HIndex)

----------------------------------------------------------------

type StaticRevIndex = Array Token StaticEntry

data StaticEntry = StaticEntry !HIndex !(Maybe ValueMap)

type ValueMap = Map HeaderValue HIndex

----------------------------------------------------------------

beg :: Token
beg = minBound

end :: Token
end = toEnum (fromEnum (maxBound :: Token) - 1)

----------------------------------------------------------------

staticRevIndex :: StaticRevIndex
staticRevIndex = A.array (minBound, end) $ map toEntry zs
  where
    toEntry (k,xs) = (toToken k, m)
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
lookupStaticRevIndex :: Token -> HeaderValue -> RevResult
lookupStaticRevIndex t v = case staticRevIndex ! t of
    StaticEntry i Nothing  -> K i
    StaticEntry i (Just m) -> case M.lookup v m of
        Nothing -> K i
        Just j  -> KV j

----------------------------------------------------------------

newDynamicRevIndex :: IO DynamicRevIndex
newDynamicRevIndex = A.listArray (beg,end) <$> mapM mk lst
  where
    mk _ = newIORef M.empty
    lst = [beg..end]

renewDynamicRevIndex :: DynamicRevIndex -> IO ()
renewDynamicRevIndex drev = mapM_ clear [beg..end]
  where
    clear t = writeIORef (drev ! t) M.empty

{-# INLINE lookupDynamicRevIndex #-}
lookupDynamicRevIndex :: Token -> HeaderValue -> DynamicRevIndex -> IO RevResult
lookupDynamicRevIndex t v drev = do
    let ref = drev ! t
    m <- readIORef ref
    return $! case M.lookup v m of
        Nothing -> N
        Just i  -> KV i

{-# INLINE insertDynamicRevIndex #-}
insertDynamicRevIndex :: Token -> HeaderValue -> HIndex -> DynamicRevIndex -> IO ()
insertDynamicRevIndex t v i drev = modifyIORef ref $ M.insert v i
  where
    ref = drev ! t

{-# INLINE deleteDynamicRevIndex#-}
deleteDynamicRevIndex :: Token -> HeaderValue -> DynamicRevIndex -> IO ()
deleteDynamicRevIndex t v drev = modifyIORef ref $ M.delete v
  where
    ref = drev ! t

----------------------------------------------------------------

newOtherRevIndex :: IO OtherRevIdex
newOtherRevIndex = newIORef M.empty

renewOtherRevIndex :: OtherRevIdex -> IO ()
renewOtherRevIndex ref = writeIORef ref M.empty

{-# INLINE lookupOtherRevIndex #-}
lookupOtherRevIndex :: HeaderName -> HeaderValue -> OtherRevIdex -> IO RevResult
lookupOtherRevIndex k v ref = do
      oth <- readIORef ref
      return $! case M.lookup (k,v) oth of
          Nothing -> N
          Just i  -> KV i

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

lookupRevIndex :: HeaderName -> HeaderValue -> RevIndex -> IO RevResult
lookupRevIndex k v (RevIndex dyn oth)
  | t == TOTHER = lookupOtherRevIndex k v oth
  | otherwise   = do
      mx <- lookupDynamicRevIndex t v dyn
      return $! case mx of
          N -> lookupStaticRevIndex t v
          _ -> mx
  where
    t = toToken k

----------------------------------------------------------------

insertRevIndex :: Header -> HIndex -> RevIndex -> IO ()
insertRevIndex (k,v) i (RevIndex dyn oth)
  | t == TOTHER = insertOtherRevIndex k v i oth
  | otherwise   = insertDynamicRevIndex t v i dyn
  where
    t = toToken k

deleteRevIndex :: RevIndex -> Header -> IO ()
deleteRevIndex (RevIndex dyn oth) (k,v)
  | t == TOTHER = deleteOtherRevIndex k v oth
  | otherwise   = deleteDynamicRevIndex t v dyn
  where
    t = toToken k

deleteRevIndexList :: [Header] -> RevIndex -> IO ()
deleteRevIndexList hs rev = mapM_ (deleteRevIndex rev) hs
