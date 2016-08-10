{-# LANGUAGE BangPatterns, OverloadedStrings, CPP, RecordWildCards #-}

module Network.HPACK.Table.RevIndex (
    lookupRevIndex
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>), (<*>))
#endif
import Data.Array.Base (unsafeRead, unsafeAt)
import Data.Array.IO (IOArray)
import Data.IORef
import Network.HPACK.Table.Dynamic
import Network.HPACK.Table.Entry
import Network.HPACK.Table.Static
import Network.HPACK.Types

----------------------------------------------------------------

data Rev = N | K HIndex | KV HIndex deriving Eq

lookupStatic :: HeaderName -> HeaderValue -> DynamicTable -> IO Rev
lookupStatic k v DynamicTable{..} = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    n <- readIORef numOfEntries
    let !beg = off + 1
        !end = off + n
    tbl <- readIORef circularTable
    goD beg end maxN tbl N
  where
    goD :: Int -> Int -> Int -> IOArray Index Entry -> Rev -> IO Rev
    goD i end maxN tbl r
      | i > end   = goS 1 r
      | otherwise = do
            Entry _ (k',v') <- unsafeRead tbl ((i + maxN) `mod` maxN)
            let !i' = i + 1
            if k == k' then
                if v == v' then
                    return $ KV (DIndex i)
                  else
                    goD i' end maxN tbl (K (DIndex i))
              else
                goD i' end maxN tbl r
    goS i r
      | i > staticTableSize = return r
      | otherwise           = do
            let (k',v') = staticTable' `unsafeAt` (i - 1)
                !i' = i + 1
            if k == k' then
                if v == v' then
                    return $ KV (SIndex i)
                  else
                    goS i' (K (SIndex i))
              else
                goS i' r

lookupRevIndex :: HeaderName
               -> HeaderValue
               -> (HIndex -> IO ())
               -> (HeaderValue -> Entry -> HIndex -> IO ())
               -> (HeaderName -> HeaderValue -> Entry -> IO ())
               -> (HeaderValue -> HIndex -> IO ())
               -> DynamicTable
               -> IO ()
lookupRevIndex k v fa fb fc fd dyntbl = do
    r <- lookupStatic k v dyntbl
    case r of
      N                   -> fc k v ent
      K i
        | shouldBeIndexed -> fb v ent i
        | otherwise       -> fd v i
      KV i                -> fa i
  where
    shouldBeIndexed = k `notElem` headersNotToIndex
    ent = toEntry (k,v)

----------------------------------------------------------------

headersNotToIndex :: [HeaderName]
headersNotToIndex = [
    ":path"
  , "content-length"
  , "location"
  , "etag"
  , "set-cookie"
  ]

