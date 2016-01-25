{-# LANGUAGE TupleSections, RecordWildCards, FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Network.HPACK2.Table.Dynamic (
    DynamicTable(..)
  , newDynamicTableForEncoding
  , newDynamicTableForDecoding
  , renewDynamicTable
  , printDynamicTable
  , isDynamicTableEmpty
  , isSuitableSize
  , TableSizeAction(..)
  , needChangeTableSize
  , setLimitForEncoding
  , resetLimitForEncoding
  , insertEntry
  , toHeaderEntry
  , fromDIndexToIndex
  , fromIndexToDIndex
  ) where

import Control.Monad (forM)
import Data.Array.IO (IOArray, newArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Network.HPACK2.Table.Entry
import Network.HPACK2.Table.RevIndex
import Network.HPACK2.Table.Static

----------------------------------------------------------------

{-# INLINE fromDIndexToIndex #-}
fromDIndexToIndex :: DynamicTable -> DIndex -> IO Index
fromDIndexToIndex DynamicTable{..} (DIndex didx) = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    return $! adj maxN (didx - off) + staticTableSize

{-# INLINE fromIndexToDIndex #-}
fromIndexToDIndex :: DynamicTable -> Index -> IO DIndex
fromIndexToDIndex DynamicTable{..} idx = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    let !didx = adj maxN (idx + off - staticTableSize)
    return $! DIndex didx

----------------------------------------------------------------

type Table = IOArray Index Entry

{-
        offset
        v
   +-+-+-+-+-+-+-+-+
   | | | |z|y|x| | |
   +-+-+-+-+-+-+-+-+
          1 2 3      (numOfEntries = 3)

After insertion:

      offset
      v
   +-+-+-+-+-+-+-+-+
   | | |w|z|y|x| | |
   +-+-+-+-+-+-+-+-+
        1 2 3 4      (numOfEntries = 4)
-}

-- | Type for dynamic table.
data DynamicTable = DynamicTable {
  -- | An array
    circularTable :: !(IORef Table)
  -- | Start point
  , offset :: !(IORef Index)
  -- | The current number of entries
  , numOfEntries :: !(IORef Int)
  -- | The size of the array
  , maxNumOfEntries :: !(IORef Int)
  -- | The current dynamic table size (defined in HPACK)
  , dynamicTableSize :: !(IORef Size)
  -- | The max dynamic table size (defined in HPACK)
  , maxDynamicTableSize :: !(IORef Size)
  -- | The value informed by SETTINGS_HEADER_TABLE_SIZE.
  --   If 'Nothing', dynamic table size update is not necessary.
  --   Otherwise, dynamic table size update is sent
  --   and this value should be set to 'Nothing'.
  , limitForEncoding :: !(IORef (Maybe Size))
  -- | The limit size of a dynamic table for decoding
  , limitForDecoding :: !(IORef Size)
  -- | Header to the index in Dynamic Table for encoder.
  --   Static Table is not included.
  --   Nothing for decoder.
  , reverseIndex :: !(IORef (Maybe Outer))
  }

{-# INLINE adj #-}
adj :: Int -> Int -> Int
adj maxN x = (x + maxN) `mod` maxN

----------------------------------------------------------------

-- | Printing 'DynamicTable'.
printDynamicTable :: DynamicTable -> IO ()
printDynamicTable DynamicTable{..} = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    n <- readIORef numOfEntries
    let beg = off + 1
        end = off + n
    tbl <- readIORef circularTable
    es <- mapM (readArray tbl . adj maxN) [beg .. end]
    let ts = zip [1..] es
    mapM_ printEntry ts
    dsize <- readIORef dynamicTableSize
    maxdsize <- readIORef maxDynamicTableSize
    putStrLn $ "      Table size: " ++ show dsize ++ "/" ++ show maxdsize
    rev <- readIORef reverseIndex
    print rev

printEntry :: (Index,Entry) -> IO ()
printEntry (i,e) = do
    putStr "[ "
    putStr $ show i
    putStr "] (s = "
    putStr $ show $ entrySize e
    putStr ") "
    BS.putStr $ entryHeaderName e
    putStr ": "
    BS.putStrLn $ entryHeaderValue e

----------------------------------------------------------------

isDynamicTableEmpty :: DynamicTable -> IO Bool
isDynamicTableEmpty DynamicTable{..} = do
    n <- readIORef numOfEntries
    return $! n == 0

isSuitableSize :: Size -> DynamicTable -> IO Bool
isSuitableSize siz DynamicTable{..} = do
    lim <- readIORef limitForDecoding
    return $! siz <= lim

data TableSizeAction = Keep | Change !Size | Ignore !Size

needChangeTableSize :: DynamicTable -> IO TableSizeAction
needChangeTableSize DynamicTable{..} = do
    mlim <- readIORef limitForEncoding
    maxsiz <- readIORef maxDynamicTableSize
    return $ case mlim of
        Nothing          -> Keep
        Just lim
          | lim < maxsiz -> Change lim
          | otherwise    -> Ignore maxsiz

-- | When SETTINGS_HEADER_TABLE_SIZE is received from a peer,
--   its value should be set by this function.
setLimitForEncoding :: Size -> DynamicTable -> IO ()
setLimitForEncoding siz DynamicTable{..} = writeIORef limitForEncoding $ Just siz

resetLimitForEncoding :: DynamicTable -> IO ()
resetLimitForEncoding DynamicTable{..} = writeIORef limitForEncoding Nothing

----------------------------------------------------------------

-- | Creating 'DynamicTable'.
newDynamicTableForEncoding :: Size -> IO DynamicTable
newDynamicTableForEncoding maxsiz = newDynamicTable maxsiz maxsiz (Just defaultRevIndex)

-- | Creating 'DynamicTable'.
newDynamicTableForDecoding :: Size -> IO DynamicTable
newDynamicTableForDecoding maxsiz = newDynamicTable maxsiz maxsiz Nothing

newDynamicTable :: Size -> Size -> Maybe Outer -> IO DynamicTable
newDynamicTable maxsiz dlim mhp = do
    tbl <- newArray (0,end) dummyEntry
    DynamicTable <$> newIORef tbl     -- circularTable
                 <*> newIORef end     -- offset
                 <*> newIORef 0       -- numOfEntries
                 <*> newIORef maxN    -- maxNumOfEntries
                 <*> newIORef 0       -- dynamicTableSize
                 <*> newIORef maxsiz  -- maxDynamicTableSize
                 <*> newIORef Nothing -- limitForEncoding
                 <*> newIORef dlim    -- limitForDecoding
                 <*> newIORef mhp     -- reverseIndex
  where
    maxN = maxNumbers maxsiz
    end = maxN - 1

-- | Renewing 'DynamicTable' with necessary entries copied.
renewDynamicTable :: Size -> DynamicTable -> IO DynamicTable -- fixme
renewDynamicTable maxsiz olddyntbl = do
    putStrLn "renew!!!!"
    renew <- shouldRenew olddyntbl maxsiz
    dlim <- readIORef $ limitForDecoding olddyntbl
    orev <- readIORef $ reverseIndex olddyntbl
    let mhp = case orev of
            Nothing -> Nothing
            _       -> Just defaultRevIndex -- fixme
    if renew then
        newDynamicTable maxsiz dlim mhp >>= copyTable olddyntbl
      else
        return olddyntbl

copyTable :: DynamicTable -> DynamicTable -> IO DynamicTable
copyTable olddyntbl newdyntbl = getEntries olddyntbl >>= copyEntries newdyntbl

getEntries :: DynamicTable -> IO [Entry]
getEntries DynamicTable{..} = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    table <- readIORef circularTable
    let readTable i = readArray table $ adj maxN (off + i)
    forM [1 .. maxN] readTable

copyEntries :: DynamicTable -> [Entry] -> IO DynamicTable
copyEntries dyntbl                 [] = return dyntbl
copyEntries dyntbl@DynamicTable{..} (e:es) = do
    dsize <- readIORef dynamicTableSize
    maxdsize <- readIORef maxDynamicTableSize
    if dsize + entrySize e <= maxdsize then do
        insertEnd e dyntbl
        copyEntries dyntbl es
      else
        return dyntbl

-- | Is the size of 'DynamicTable' really changed?
shouldRenew :: DynamicTable -> Size -> IO Bool
shouldRenew DynamicTable{..} maxsiz = do
    maxdsize <- readIORef maxDynamicTableSize
    return $! maxdsize /= maxsiz

----------------------------------------------------------------

-- | Inserting 'Entry' to 'DynamicTable'.
--   New 'DynamicTable', the largest new 'Index'
--   and a set of dropped OLD 'Index'
--   are returned.
insertEntry :: Entry -> DynamicTable -> IO ()
insertEntry e dyntbl@DynamicTable{..} = do
    insertFront e dyntbl
    hs <- adjustTableSize dyntbl
    mrev <- readIORef reverseIndex
    case mrev of
        Nothing  -> return ()
        Just rev -> writeIORef reverseIndex $ Just (deleteDynamicList hs rev)

insertFront :: Entry -> DynamicTable -> IO ()
insertFront e DynamicTable{..} = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    n <- readIORef numOfEntries
    dsize <- readIORef dynamicTableSize
    mrev <- readIORef reverseIndex
    table <- readIORef circularTable
    let i = off
        !dsize' = dsize + entrySize e
        !off' = adj maxN (off - 1)
        !mrev' = case mrev of
            Nothing  -> Nothing
            Just rev -> Just $ insertDynamic (entryHeader e) (DIndex i) rev
    writeArray table i e
    writeIORef offset off'
    writeIORef numOfEntries $ n + 1
    writeIORef dynamicTableSize dsize'
    writeIORef reverseIndex mrev'

adjustTableSize :: DynamicTable -> IO [Header]
adjustTableSize dyntbl = adjust dyntbl []

adjust :: DynamicTable -> [Header] -> IO [Header]
adjust dyntbl@DynamicTable{..} !hs = do
    dsize <- readIORef dynamicTableSize
    maxdsize <- readIORef maxDynamicTableSize
    if dsize <= maxdsize then
        return hs
      else do
        h <- removeEnd dyntbl
        adjust dyntbl (h:hs)

----------------------------------------------------------------

insertEnd :: Entry -> DynamicTable -> IO ()
insertEnd e DynamicTable{..} = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    n <- readIORef numOfEntries
    dsize <- readIORef dynamicTableSize
    mrev <- readIORef reverseIndex
    table <- readIORef circularTable
    let i = adj maxN (off + n + 1)
        dsize' = dsize + entrySize e
        mrev' = case mrev of
            Nothing  -> Nothing
            Just rev -> Just $ insertDynamic (entryHeader e) (DIndex i) rev
    writeArray table i e
    writeIORef numOfEntries $ n + 1
    writeIORef dynamicTableSize dsize'
    writeIORef reverseIndex mrev'

----------------------------------------------------------------

removeEnd :: DynamicTable -> IO Header
removeEnd DynamicTable{..} = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    n <- readIORef numOfEntries
    let i = adj maxN (off + n)
    table <- readIORef circularTable
    e <- readArray table i
    writeArray table i dummyEntry -- let the entry GCed
    dsize <- readIORef dynamicTableSize
    let dsize' = dsize - entrySize e
        h = entryHeader e
    writeIORef numOfEntries (n - 1)
    writeIORef dynamicTableSize dsize'
    return h

----------------------------------------------------------------

toHeaderEntry :: DynamicTable -> DIndex -> IO Entry
toHeaderEntry DynamicTable{..} (DIndex didx) = do
    table <- readIORef circularTable
    readArray table didx
