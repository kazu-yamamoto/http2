{-# LANGUAGE TupleSections, RecordWildCards, FlexibleContexts #-}

module Network.HPACK.Table.Dynamic (
    DynamicTable(..)
  , newDynamicTableForEncoding
  , newDynamicTableForDecoding
  , renewDynamicTable
  , printDynamicTable
  , isDynamicTableEmpty
  , isSuitableSize
  , insertEntry
  , toHeaderEntry
  , fromHIndexToIndex
  , fromIndexToHIndex
  , fromSIndexToIndex
  , fromIndexToSIndex
  ) where

import Data.Array.IO (IOArray, newArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as BS
import Network.HPACK.Table.Entry
import qualified Network.HPACK.Table.DoubleHashMap as DHM
import Network.HPACK.Table.Static
import Control.Monad (forM)

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
  -- | The limit size of a dynamic table for decoding
  , limitForDecoding :: !Size
  -- | Header to the index in Dynamic Table for encoder.
  --   Static Table is not included.
  --   Nothing for decoder.
  , reverseIndex :: Maybe (DHM.DoubleHashMap HIndex)
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
isDynamicTableEmpty hdrtbl = numOfEntries hdrtbl == 0

isSuitableSize :: Size -> DynamicTable -> Bool
isSuitableSize siz tbl = siz <= limitForDecoding tbl

----------------------------------------------------------------

-- Physical array index for Dynamic Table.
newtype HIndex = HIndex Int deriving (Eq, Ord, Show)

----------------------------------------------------------------

fromHIndexToIndex :: DynamicTable -> HIndex -> Index
fromHIndexToIndex DynamicTable{..} (HIndex hidx) = idx
  where
    idx = adj maxNumOfEntries (hidx - offset) + staticTableSize

fromIndexToHIndex :: DynamicTable -> Index -> HIndex
fromIndexToHIndex DynamicTable{..} idx = HIndex hidx
  where
    hidx = adj maxNumOfEntries (idx + offset - staticTableSize)

fromSIndexToIndex :: DynamicTable -> SIndex -> Index
fromSIndexToIndex _ sidx = fromStaticIndex sidx

fromIndexToSIndex :: DynamicTable -> Index -> SIndex
fromIndexToSIndex _ idx = toStaticIndex idx

----------------------------------------------------------------

-- | Creating 'DynamicTable'.
newDynamicTableForEncoding :: Size -> IO DynamicTable
newDynamicTableForEncoding maxsiz = newDynamicTable maxsiz maxsiz (Just DHM.empty)

-- | Creating 'DynamicTable'.
newDynamicTableForDecoding :: Size -> IO DynamicTable
newDynamicTableForDecoding maxsiz = newDynamicTable maxsiz maxsiz Nothing

newDynamicTable :: Size -> Size -> Maybe (DHM.DoubleHashMap HIndex) -> IO DynamicTable
newDynamicTable maxsiz dlim mhp = do
    tbl <- newArray (0,end) dummyEntry
    return DynamicTable {
        maxNumOfEntries = maxN
      , offset = end
      , numOfEntries = 0
      , circularTable = tbl
      , headerTableSize = 0
      , maxDynamicTableSize = maxsiz
      , limitForDecoding = dlim
      , reverseIndex = mhp
      }
  where
    maxN = maxNumbers maxsiz
    end = maxN - 1

-- | Renewing 'DynamicTable' with necessary entries copied.
renewDynamicTable :: Size -> DynamicTable -> IO DynamicTable
renewDynamicTable maxsiz oldhdrtbl | shouldRenew oldhdrtbl maxsiz =
    newDynamicTable maxsiz dlim mhp >>= copyTable oldhdrtbl
  where
    dlim = limitForDecoding oldhdrtbl
    mhp = case reverseIndex oldhdrtbl of
        Nothing -> Nothing
        _       -> Just DHM.empty
renewDynamicTable _ oldhdrtbl = return oldhdrtbl

copyTable :: DynamicTable -> DynamicTable -> IO DynamicTable
copyTable oldhdrtbl newhdrtbl = getEntries oldhdrtbl >>= copyEntries newhdrtbl

getEntries :: DynamicTable -> IO [Entry]
getEntries DynamicTable{..} = forM [1 .. numOfEntries] readTable
  where
    readTable i = readArray circularTable $ adj maxNumOfEntries (offset + i)

copyEntries :: DynamicTable -> [Entry] -> IO DynamicTable
copyEntries hdrtbl                 [] = return hdrtbl
copyEntries hdrtbl@DynamicTable{..} (e:es)
  | headerTableSize + entrySize e <= maxDynamicTableSize = do
      hdrtbl' <- insertEnd e hdrtbl
      copyEntries hdrtbl' es
  | otherwise = return hdrtbl

-- | Is the size of 'DynamicTable' really changed?
shouldRenew :: DynamicTable -> Size -> Bool
shouldRenew DynamicTable{..} maxsiz = maxDynamicTableSize /= maxsiz

----------------------------------------------------------------

-- | Inserting 'Entry' to 'DynamicTable'.
--   New 'DynamicTable', the largest new 'Index'
--   and a set of dropped OLD 'Index'
--   are returned.
insertEntry :: Entry -> DynamicTable -> IO DynamicTable
insertEntry e hdrtbl = do
    (hdrtbl', hs) <- insertFront e hdrtbl >>= adjustTableSize
    let hdrtbl'' = case reverseIndex hdrtbl' of
            Nothing  -> hdrtbl'
            Just rev -> hdrtbl' { reverseIndex = Just (DHM.deleteList hs rev) }
    return hdrtbl''

insertFront :: Entry -> DynamicTable -> IO DynamicTable
insertFront e hdrtbl@DynamicTable{..} = do
    writeArray circularTable i e
    return $ hdrtbl {
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
        Just rev -> Just $ DHM.insert (entryHeader e) (HIndex i) rev

adjustTableSize :: DynamicTable -> IO (DynamicTable, [Header])
adjustTableSize hdrtbl = adjust hdrtbl []

adjust :: DynamicTable -> [Header] -> IO (DynamicTable, [Header])
adjust hdrtbl@DynamicTable{..} hs
  | headerTableSize <= maxDynamicTableSize = return (hdrtbl, hs)
  | otherwise         = do
      (hdrtbl', h) <- removeEnd hdrtbl
      adjust hdrtbl' (h:hs)

----------------------------------------------------------------

insertEnd :: Entry -> DynamicTable -> IO DynamicTable
insertEnd e hdrtbl@DynamicTable{..} = do
    writeArray circularTable i e
    return $ hdrtbl {
        numOfEntries = numOfEntries + 1
      , headerTableSize = headerTableSize'
      , reverseIndex = reverseIndex'
      }
  where
    i = adj maxNumOfEntries (offset + numOfEntries + 1)
    headerTableSize' = headerTableSize + entrySize e
    reverseIndex' = case reverseIndex of
        Nothing  -> Nothing
        Just rev -> Just $ DHM.insert (entryHeader e) (HIndex i) rev

----------------------------------------------------------------

removeEnd :: DynamicTable -> IO (DynamicTable,Header)
removeEnd hdrtbl@DynamicTable{..} = do
    let i = adj maxNumOfEntries (offset + numOfEntries)
    e <- readArray circularTable i
    writeArray circularTable i dummyEntry -- let the entry GCed
    let tsize = headerTableSize - entrySize e
        h = entryHeader e
        hdrtbl' = hdrtbl {
            numOfEntries = numOfEntries - 1
          , headerTableSize = tsize
          }
    return (hdrtbl', h)

----------------------------------------------------------------

toHeaderEntry :: DynamicTable -> HIndex -> IO Entry
toHeaderEntry DynamicTable{..} (HIndex hidx) = readArray circularTable hidx
