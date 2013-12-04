module Network.HPACK.HeaderTable (
    HeaderTable(..)
  , newHeaderTable
  , insertEntry
  ) where

import Data.Array (listArray, (!))
import Data.Array.ST (runSTArray, writeArray)
import Data.Array.Unsafe (unsafeThaw)
import qualified Data.ByteString.Char8 as BS
import Network.HPACK.Entry
import Network.HPACK.Types

----------------------------------------------------------------

data HeaderTable = HeaderTable {
    maxNumOfEntries :: Int
  , offset :: Index
  , numOfEntries :: Int
  , circularTable :: Table
  , headerTableSize :: Size
  , maxHeaderTableSize :: Size
  }

----------------------------------------------------------------

instance Show HeaderTable where
    show (HeaderTable maxN off n tbl tblsiz _) =
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
    keyval = "(s = " ++ show s ++ ") " ++ BS.unpack k ++ ": " ++ BS.unpack v

----------------------------------------------------------------

-- maxHeaderTableSize is 4096 bytes,
-- an array has 128 entries, resulting 1024 bytes in 64bit machine
newHeaderTable :: Size -> HeaderTable
newHeaderTable maxsiz = HeaderTable {
    maxNumOfEntries = maxN
  , offset = end
  , numOfEntries = 0
  , circularTable = listArray (0, end) $ replicate maxN dummyEntry
  , headerTableSize = 0
  , maxHeaderTableSize = maxsiz
  }
  where
    maxN = maxNumbers maxsiz
    end = maxN - 1

----------------------------------------------------------------

insertEntry :: Entry -> HeaderTable -> (HeaderTable,[Index])
insertEntry e hdrtbl = adjustTableSize $ insertOne e hdrtbl

insertOne :: Entry -> HeaderTable -> HeaderTable
insertOne e hdrtbl@(HeaderTable maxN off n tbl tsize _) = hdrtbl'
  where
    i = off
    tbl' = modifyTable tbl i e
    tsize' = tsize + entrySize e
    off' = (off - 1 + maxN) `mod` maxN
    hdrtbl' = hdrtbl {
        offset = off'
      , numOfEntries = n + 1
      , circularTable = tbl'
      , headerTableSize = tsize'
      }

adjustTableSize :: HeaderTable -> (HeaderTable, [Index])
adjustTableSize hdrtbl = adjust hdrtbl []

adjust :: HeaderTable -> [Index] -> (HeaderTable, [Index])
adjust hdrtbl is
  | tsize <= maxtsize = (hdrtbl, is)
  | otherwise         = let (hdrtbl', i) = removeOne hdrtbl
                        in adjust hdrtbl' (i:is)
  where
    tsize = headerTableSize hdrtbl
    maxtsize = maxHeaderTableSize hdrtbl

removeOne :: HeaderTable -> (HeaderTable,Index)
removeOne hdrtbl@(HeaderTable maxN off n tbl tsize _) = (hdrtbl',i)
  where
    i = (off + n + maxN) `mod` maxN
    e = tbl ! i
    tbl' = modifyTable tbl i dummyEntry -- let the entry GCed
    tsize' = tsize - entrySize e
    hdrtbl' = hdrtbl {
        numOfEntries = n - 1
      , circularTable = tbl'
      , headerTableSize = tsize'
      }

modifyTable :: Table -> Index -> Entry -> Table
modifyTable tbl i e = runSTArray $ do
    arr <- unsafeThaw tbl
    writeArray arr i e
    return arr
