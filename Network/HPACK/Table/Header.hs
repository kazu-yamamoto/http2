module Network.HPACK.Table.Header (
  -- * Type
    HeaderTable(..)
  , newHeaderTable
  , printHeaderTable
  -- * Utilities
  , insertEntry
  ) where

import Data.Array.IO (IOArray, newArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as BS
import Network.HPACK.Table.Entry
import Network.HPACK.Types

----------------------------------------------------------------

-- | Type for header table.
data HeaderTable = HeaderTable {
    maxNumOfEntries :: Int
  , offset :: Index
  , numOfEntries :: Int
  , circularTable :: !(IOArray Index Entry)
  , headerTableSize :: Size
  , maxHeaderTableSize :: Size
  }

----------------------------------------------------------------

-- | Printing 'HeaderTable'.
printHeaderTable :: HeaderTable -> IO ()
printHeaderTable (HeaderTable maxN off n tbl tblsiz _) = do
    es <- mapM (readArray tbl . adj) [beg .. end]
    let ts = zip [1..] es
    mapM_ printEntry ts
    putStrLn $ "      Table size: " ++ show tblsiz
  where
    adj x = (x + maxN) `mod` maxN
    beg = off + 1
    end = off + n

printEntry :: (Index,Entry) -> IO ()
printEntry (i,e) = do
    putStr "[ "
    putStr $ show i
    putStr "] (s = "
    putStr $ show $ entrySize e
    putStr ") "
    BS.putStr $ fromHeaderName $ entryHeaderName e
    putStr ": "
    BS.putStrLn $ entryHeaderValue e

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
    return (hdrtbl',n)
