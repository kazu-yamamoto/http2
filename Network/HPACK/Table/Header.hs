module Network.HPACK.Table.Header (
  -- * Type
    HeaderTable(..)
  , newHeaderTable
--   , showHeaderTable FIXME
  -- * Utilities
  , insertEntry
  ) where

import Data.Array.IO (IOArray, newArray, readArray, writeArray)
-- import qualified Data.ByteString.Char8 as BS
-- import Data.CaseInsensitive (foldedCase)
import Network.HPACK.Table.Entry

----------------------------------------------------------------

-- | Type for header table.
data HeaderTable = HeaderTable {
    maxNumOfEntries :: Int
  , offset :: Index
  , numOfEntries :: Int
  , circularTable :: IOArray Index Entry
  , headerTableSize :: Size
  , maxHeaderTableSize :: Size
  } -- deriving Show -- FIXME

----------------------------------------------------------------

{- FIXME
-- | Converting 'HeaderTable' to 'String'.
showHeaderTable :: HeaderTable -> String
showHeaderTable (HeaderTable maxN off n tbl tblsiz _) =
        showArray tbl (\x -> (x + maxN) `mod` maxN) (off+1) n
     ++ "      Table size: " ++ show tblsiz

showArray :: Table -> (Index -> Index) -> Index -> Int -> String
showArray tbl adj off n = showArray' tbl adj off n 1

showArray' :: Table -> (Index -> Index) -> Index -> Int -> Int -> String
showArray' tbl adj off n cnt
  | cnt > n   = ""
  | otherwise = "[ " ++ show cnt ++ "] " ++ keyval ++ "\n"
             ++ showArray' tbl adj (off+1) n (cnt+1)
  where
    (s,(k,v)) = tbl ! (adj off)
    keyval = "(s = " ++ show s ++ ") " ++ BS.unpack (foldedCase k) ++ ": " ++ BS.unpack v
-}

----------------------------------------------------------------

-- | Creating 'HeaderTable'.
-- The default maxHeaderTableSize is 4096 bytes,
-- an array has 128 entries, resulting 1024 bytes in 64bit machine
newHeaderTable :: Size -> IO HeaderTable
newHeaderTable maxsiz = do
    tbl <- newArray (0,end) dummyEntry
    return HeaderTable {
        maxNumOfEntries = maxN
      , offset = end
      , numOfEntries = 0
      , circularTable = tbl
      , headerTableSize = 0
      , maxHeaderTableSize = maxsiz
      }
  where
    maxN = maxNumbers maxsiz
    end = maxN - 1

----------------------------------------------------------------

-- | Inserting 'Entry' to 'HeaderTable'.
--   New 'HeaderTable' and a set of dropped 'Index'
--   are returned.
insertEntry :: Entry -> HeaderTable -> IO (HeaderTable,[Index])
insertEntry e hdrtbl = insertOne e hdrtbl >>= adjustTableSize

insertOne :: Entry -> HeaderTable -> IO HeaderTable
insertOne e hdrtbl@(HeaderTable maxN off n tbl tsize _) = do
    writeArray tbl i e
    return $ hdrtbl {
        offset = off'
      , numOfEntries = n + 1
      , headerTableSize = tsize'
      }
  where
    i = off
    tsize' = tsize + entrySize e
    off' = (off - 1 + maxN) `mod` maxN

adjustTableSize :: HeaderTable -> IO (HeaderTable, [Index])
adjustTableSize hdrtbl = adjust hdrtbl []

adjust :: HeaderTable -> [Index] -> IO (HeaderTable, [Index])
adjust hdrtbl is
  | tsize <= maxtsize = return (hdrtbl, is)
  | otherwise         = do
      (hdrtbl', i) <- removeOne hdrtbl
      adjust hdrtbl' (i:is)
  where
    tsize = headerTableSize hdrtbl
    maxtsize = maxHeaderTableSize hdrtbl

removeOne :: HeaderTable -> IO (HeaderTable,Index)
removeOne hdrtbl@(HeaderTable maxN off n tbl tsize _) = do
    let i = (off + n + maxN) `mod` maxN
    e <- readArray tbl i
    writeArray tbl i dummyEntry -- let the entry GCed
    let tsize' = tsize - entrySize e
    let hdrtbl' = hdrtbl {
            numOfEntries = n - 1
          , headerTableSize = tsize'
          }
    return (hdrtbl',i)
