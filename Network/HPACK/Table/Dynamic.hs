{-# LANGUAGE TupleSections, RecordWildCards, FlexibleContexts #-}

module Network.HPACK.Table.Dynamic (
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
import Network.HPACK.Table.Entry
import Network.HPACK.Table.RevIndex
import Network.HPACK.Table.Static

----------------------------------------------------------------

{-# INLINE fromDIndexToIndex #-}
fromDIndexToIndex :: DynamicTable -> DIndex -> Index
fromDIndexToIndex DynamicTable{..} (DIndex didx) = idx
  where
    idx = adj maxNumOfEntries (didx - offset) + staticTableSize

{-# INLINE fromIndexToDIndex #-}
fromIndexToDIndex :: DynamicTable -> Index -> DIndex
fromIndexToDIndex DynamicTable{..} idx = DIndex didx
  where
    didx = adj maxNumOfEntries (idx + offset - staticTableSize)

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
    circularTable :: !Table
  -- | Start point
  , offset :: !Index
  -- | The current number of entries
  , numOfEntries :: !Int
  -- | The size of the array
  , maxNumOfEntries :: !Int
  -- | The current dynamic table size (defined in HPACK)
  , headerTableSize :: !Size
  -- | The max dynamic table size (defined in HPACK)
  , maxDynamicTableSize :: !Size
  -- | The value informed by SETTINGS_HEADER_TABLE_SIZE.
  --   If 'Nothing', dynamic table size update is not necessary.
  --   Otherwise, dynamic table size update is sent
  --   and this value should be set to 'Nothing'.
  , limitForEncoding :: !(IORef (Maybe Size))
  -- | The limit size of a dynamic table for decoding
  , limitForDecoding :: !Size
  -- | Header to the index in Dynamic Table for encoder.
  --   Static Table is not included.
  --   Nothing for decoder.
  , reverseIndex :: !(Maybe Outer)
  }

adj :: Int -> Int -> Int
adj maxN x = (x + maxN) `mod` maxN

----------------------------------------------------------------

-- | Printing 'DynamicTable'.
printDynamicTable :: DynamicTable -> IO ()
printDynamicTable DynamicTable{..} = do
    es <- mapM (readArray circularTable . adj maxNumOfEntries) [beg .. end]
    let ts = zip [1..] es
    mapM_ printEntry ts
    putStrLn $ "      Table size: " ++ show headerTableSize ++ "/" ++ show maxDynamicTableSize
    print reverseIndex
  where
    beg = offset + 1
    end = offset + numOfEntries

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

isDynamicTableEmpty :: DynamicTable -> Bool
isDynamicTableEmpty dyntbl = numOfEntries dyntbl == 0

isSuitableSize :: Size -> DynamicTable -> Bool
isSuitableSize siz tbl = siz <= limitForDecoding tbl

data TableSizeAction = Keep | Change !Size | Ignore !Size

needChangeTableSize :: DynamicTable -> IO TableSizeAction
needChangeTableSize tbl = do
    mlim <- getLimitForEncoding tbl
    return $ case mlim of
        Nothing          -> Keep
        Just lim
          | lim < maxsiz -> Change lim
          | otherwise    -> Ignore maxsiz
  where
    maxsiz = maxDynamicTableSize tbl

getLimitForEncoding :: DynamicTable -> IO (Maybe Size)
getLimitForEncoding dyntbl = readIORef $ limitForEncoding dyntbl

-- | When SETTINGS_HEADER_TABLE_SIZE is received from a peer,
--   its value should be set by this function.
setLimitForEncoding :: Size -> DynamicTable -> IO ()
setLimitForEncoding siz dyntbl = writeIORef (limitForEncoding dyntbl) $ Just siz

resetLimitForEncoding :: DynamicTable -> IO ()
resetLimitForEncoding dyntbl = writeIORef (limitForEncoding dyntbl) Nothing

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
    lim <- newIORef Nothing
    return DynamicTable {
        maxNumOfEntries = maxN
      , offset = end
      , numOfEntries = 0
      , circularTable = tbl
      , headerTableSize = 0
      , maxDynamicTableSize = maxsiz
      , limitForEncoding = lim
      , limitForDecoding = dlim
      , reverseIndex = mhp
      }
  where
    maxN = maxNumbers maxsiz
    end = maxN - 1

-- | Renewing 'DynamicTable' with necessary entries copied.
renewDynamicTable :: Size -> DynamicTable -> IO DynamicTable
renewDynamicTable maxsiz olddyntbl | shouldRenew olddyntbl maxsiz =
    newDynamicTable maxsiz dlim mhp >>= copyTable olddyntbl
  where
    dlim = limitForDecoding olddyntbl
    mhp = case reverseIndex olddyntbl of
        Nothing -> Nothing
        _       -> Just defaultRevIndex
renewDynamicTable _ olddyntbl = return olddyntbl

copyTable :: DynamicTable -> DynamicTable -> IO DynamicTable
copyTable olddyntbl newdyntbl = getEntries olddyntbl >>= copyEntries newdyntbl

getEntries :: DynamicTable -> IO [Entry]
getEntries DynamicTable{..} = forM [1 .. numOfEntries] readTable
  where
    readTable i = readArray circularTable $ adj maxNumOfEntries (offset + i)

copyEntries :: DynamicTable -> [Entry] -> IO DynamicTable
copyEntries dyntbl                 [] = return dyntbl
copyEntries dyntbl@DynamicTable{..} (e:es)
  | headerTableSize + entrySize e <= maxDynamicTableSize = do
      dyntbl' <- insertEnd e dyntbl
      copyEntries dyntbl' es
  | otherwise = return dyntbl

-- | Is the size of 'DynamicTable' really changed?
shouldRenew :: DynamicTable -> Size -> Bool
shouldRenew DynamicTable{..} maxsiz = maxDynamicTableSize /= maxsiz

----------------------------------------------------------------

-- | Inserting 'Entry' to 'DynamicTable'.
--   New 'DynamicTable', the largest new 'Index'
--   and a set of dropped OLD 'Index'
--   are returned.
insertEntry :: Entry -> DynamicTable -> IO DynamicTable
insertEntry e dyntbl = do
    (dyntbl', hs) <- insertFront e dyntbl >>= adjustTableSize
    let dyntbl'' = case reverseIndex dyntbl' of
            Nothing  -> dyntbl'
            Just rev -> dyntbl' { reverseIndex = Just (deleteDynamicList hs rev) }
    return dyntbl''

insertFront :: Entry -> DynamicTable -> IO DynamicTable
insertFront e dyntbl@DynamicTable{..} = do
    writeArray circularTable i e
    return $ dyntbl {
        offset = offset'
      , numOfEntries = numOfEntries + 1
      , headerTableSize = headerTableSize'
      , reverseIndex = reverseIndex'
      }
  where
    i = offset
    headerTableSize' = headerTableSize + entrySize e
    offset' = adj maxNumOfEntries (offset - 1)
    reverseIndex' = case reverseIndex of
        Nothing  -> Nothing
        Just rev -> Just $ insertDynamic (entryHeader e) (DIndex i) rev

adjustTableSize :: DynamicTable -> IO (DynamicTable, [Header])
adjustTableSize dyntbl = adjust dyntbl []

adjust :: DynamicTable -> [Header] -> IO (DynamicTable, [Header])
adjust dyntbl@DynamicTable{..} hs
  | headerTableSize <= maxDynamicTableSize = return (dyntbl, hs)
  | otherwise         = do
      (dyntbl', h) <- removeEnd dyntbl
      adjust dyntbl' (h:hs)

----------------------------------------------------------------

insertEnd :: Entry -> DynamicTable -> IO DynamicTable
insertEnd e dyntbl@DynamicTable{..} = do
    writeArray circularTable i e
    return $ dyntbl {
        numOfEntries = numOfEntries + 1
      , headerTableSize = headerTableSize'
      , reverseIndex = reverseIndex'
      }
  where
    i = adj maxNumOfEntries (offset + numOfEntries + 1)
    headerTableSize' = headerTableSize + entrySize e
    reverseIndex' = case reverseIndex of
        Nothing  -> Nothing
        Just rev -> Just $ insertDynamic (entryHeader e) (DIndex i) rev

----------------------------------------------------------------

removeEnd :: DynamicTable -> IO (DynamicTable,Header)
removeEnd dyntbl@DynamicTable{..} = do
    let i = adj maxNumOfEntries (offset + numOfEntries)
    e <- readArray circularTable i
    writeArray circularTable i dummyEntry -- let the entry GCed
    let tsize = headerTableSize - entrySize e
        h = entryHeader e
        dyntbl' = dyntbl {
            numOfEntries = numOfEntries - 1
          , headerTableSize = tsize
          }
    return (dyntbl', h)

----------------------------------------------------------------

toHeaderEntry :: DynamicTable -> DIndex -> IO Entry
toHeaderEntry DynamicTable{..} (DIndex didx) = readArray circularTable didx
