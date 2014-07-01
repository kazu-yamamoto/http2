{-# LANGUAGE TupleSections, RecordWildCards #-}

module Network.HPACK.Table (
  -- * Header table
    HeaderTable
  , newHeaderTableForEncoding
  , newHeaderTableForDecoding
  , renewHeaderTable
  , printHeaderTable
  , shouldRenew
  -- * Insertion
  , insertEntry
  -- * Lookup
  , HeaderCache(..)
  , lookupTable
  -- * Entry
  , module Network.HPACK.Table.Entry
  -- * Which tables
  , WhichTable(..)
  , which
  ) where

import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import Data.Array.IO (IOArray, newArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as BS
import Network.HPACK.Table.Entry
import qualified Network.HPACK.Table.HashPSQ as HP
import Network.HPACK.Table.Static
import Network.HPACK.Types
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

-- | Type for header table.
data HeaderTable = HeaderTable {
  -- | An array
    circularTable :: !Table
  -- | Start point
  , offset :: !Index
  -- | The current number of entries
  , numOfEntries :: !Int
  -- | The size of the array
  , maxNumOfEntries :: !Int
  -- | The current header table size (defined in HPACK)
  , headerTableSize :: !Size
  -- | The max header table size (defined in HPACK)
  , maxHeaderTableSize :: !Size
  -- | Header-to-index
  , reverseIndex :: Maybe (HP.HashPSQ HIndex)
  }

adj :: Int -> Int -> Int
adj maxN x = (x + maxN) `mod` maxN

----------------------------------------------------------------

-- | Printing 'HeaderTable'.
printHeaderTable :: HeaderTable -> IO ()
printHeaderTable HeaderTable{..} = do
    es <- mapM (readArray circularTable . adj maxNumOfEntries) [beg .. end]
    let ts = zip [1..] es
    mapM_ printEntry ts
    putStrLn $ "      Table size: " ++ show headerTableSize
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

-- | Which table does `Index` refer to?
data WhichTable = InHeaderTable | InStaticTable deriving (Eq,Show)

data HeaderCache = None
                 | KeyOnly WhichTable Index
                 | KeyValue WhichTable Index deriving Show

----------------------------------------------------------------

newtype HIndex = HIndex Int deriving (Eq, Ord, Show)

----------------------------------------------------------------

fromHIndexToIndex :: HeaderTable -> HIndex -> Index
fromHIndexToIndex HeaderTable{..} (HIndex hidx) = idx
  where
    idx = adj maxNumOfEntries (maxNumOfEntries + hidx - offset)

fromIndexToHIndex :: HeaderTable -> Index -> HIndex
fromIndexToHIndex HeaderTable{..} idx = HIndex hidx
  where
    hidx = adj maxNumOfEntries (offset + idx)

fromSIndexToIndex :: HeaderTable -> SIndex -> Index
fromSIndexToIndex HeaderTable{..} sidx = fromStaticIndex sidx + numOfEntries

fromIndexToSIndex :: HeaderTable -> Index -> SIndex
fromIndexToSIndex HeaderTable{..} idx = toStaticIndex sidx
  where
    sidx = idx - numOfEntries

----------------------------------------------------------------

-- | Creating 'HeaderTable'.
-- The default maxHeaderTableSize is 4096 bytes,
-- an array has 128 entries, resulting 1024 bytes in 64bit machine
newHeaderTableForEncoding :: Size -> IO HeaderTable
newHeaderTableForEncoding maxsiz = newHeaderTable maxsiz (Just HP.empty)

-- | Creating 'HeaderTable'.
-- The default maxHeaderTableSize is 4096 bytes,
-- an array has 128 entries, resulting 1024 bytes in 64bit machine
newHeaderTableForDecoding :: Size -> IO HeaderTable
newHeaderTableForDecoding maxsiz = newHeaderTable maxsiz Nothing

newHeaderTable :: Size -> Maybe (HP.HashPSQ HIndex) -> IO HeaderTable
newHeaderTable maxsiz mhp = do
    tbl <- newArray (0,end) dummyEntry
    return HeaderTable {
        maxNumOfEntries = maxN
      , offset = end
      , numOfEntries = 0
      , circularTable = tbl
      , headerTableSize = 0
      , maxHeaderTableSize = maxsiz
      , reverseIndex = mhp
      }
  where
    maxN = maxNumbers maxsiz
    end = maxN - 1

renewHeaderTable :: Size -> HeaderTable -> IO (HeaderTable, Int)
renewHeaderTable maxsiz oldhdrtbl = do
    putStrLn $ "numOfEntries oldhdrtbl: " ++ show (numOfEntries oldhdrtbl)
    hdrtbl <- newHeaderTable maxsiz mhp
    newhdrtbl <- copyTable oldhdrtbl hdrtbl
    putStrLn $ "numOfEntries newhdrtbl: " ++ show (numOfEntries newhdrtbl)
    return (newhdrtbl, numOfEntries newhdrtbl)
  where
    mhp = case reverseIndex oldhdrtbl of
        Nothing -> Nothing
        _       -> Just HP.empty

copyTable :: HeaderTable -> HeaderTable -> IO HeaderTable
copyTable oldhdrtbl newhdrtbl = do
    ents <- getEntries oldhdrtbl
    putStrLn $ "length of entries: " ++ show (length ents)
    copyEntries newhdrtbl ents

getEntries :: HeaderTable -> IO [Entry]
getEntries HeaderTable{..} = forM [1 .. numOfEntries] readTable
  where
    readTable i = readArray circularTable $ adj maxNumOfEntries (offset + i)

copyEntries :: HeaderTable -> [Entry] -> IO HeaderTable
copyEntries hdrtbl                 [] = return hdrtbl
copyEntries hdrtbl@HeaderTable{..} (e:es)
  | headerTableSize + entrySize e <= maxHeaderTableSize = do
      hdrtbl' <- insertEnd e hdrtbl
      copyEntries hdrtbl' es
  | otherwise = return hdrtbl

shouldRenew :: HeaderTable -> Size -> Bool
shouldRenew HeaderTable{..} maxsiz = maxHeaderTableSize /= maxsiz

----------------------------------------------------------------

-- | Inserting 'Entry' to 'HeaderTable'.
--   New 'HeaderTable', the largest new 'Index'
--   and a set of dropped OLD 'Index'
--   are returned.
insertEntry :: Entry -> HeaderTable -> IO (HeaderTable,Index,[Index])
insertEntry e hdrtbl = do
    (hdrtbl', is, hs) <- insertFront e hdrtbl >>= adjustTableSize
    let hdrtbl'' = case reverseIndex hdrtbl' of
            Nothing  -> hdrtbl'
            Just rev -> hdrtbl' { reverseIndex = Just (HP.deleteList hs rev) }
    return (hdrtbl'', numOfEntries hdrtbl'', is)

insertFront :: Entry -> HeaderTable -> IO HeaderTable
insertFront e hdrtbl@HeaderTable{..} = do
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
        Just rev -> Just $ HP.insert (entryHeader e) (HIndex i) rev

adjustTableSize :: HeaderTable -> IO (HeaderTable, [Index], [Header])
adjustTableSize hdrtbl = adjust hdrtbl [] []

adjust :: HeaderTable -> [Index] -> [Header] -> IO (HeaderTable, [Index], [Header])
adjust hdrtbl is hs
  | tsize <= maxtsize = return (hdrtbl, is, hs)
  | otherwise         = do
      (hdrtbl', i, h) <- removeEnd hdrtbl
      adjust hdrtbl' (i:is) (h:hs)
  where
    tsize = headerTableSize hdrtbl
    maxtsize = maxHeaderTableSize hdrtbl

----------------------------------------------------------------

insertEnd :: Entry -> HeaderTable -> IO HeaderTable
insertEnd e hdrtbl@HeaderTable{..} = do
    writeArray circularTable i e
    return $ hdrtbl {
        numOfEntries = numOfEntries + 1
      , headerTableSize = headerTableSize'
      , reverseIndex = reverseIndex'
      }
  where
    i = adj maxNumOfEntries (offset + numOfEntries + 1)
    hi = numOfEntries + 1
    headerTableSize' = headerTableSize + entrySize e
    reverseIndex' = case reverseIndex of
        Nothing  -> Nothing
        Just rev -> Just $ HP.insert (entryHeader e) (HIndex hi) rev

----------------------------------------------------------------

removeEnd :: HeaderTable -> IO (HeaderTable,Index,Header)
removeEnd hdrtbl@HeaderTable{..} = do
    let i = adj maxNumOfEntries (offset + numOfEntries)
    e <- readArray circularTable i
    writeArray circularTable i dummyEntry -- let the entry GCed
    let tsize = headerTableSize - entrySize e
        h = entryHeader e
        hdrtbl' = hdrtbl {
            numOfEntries = numOfEntries - 1
          , headerTableSize = tsize
          }
    return (hdrtbl', numOfEntries - 1, h)

----------------------------------------------------------------

lookupTable :: Header -> HeaderTable -> HeaderCache
lookupTable h hdrtbl = case mrev of
    Nothing            -> None
    Just rev -> case HP.search h rev of
        HP.N -> case HP.search h staticHashPSQ of
            HP.N       -> None
            HP.K  sidx -> KeyOnly  InStaticTable (fromSIndexToIndex hdrtbl sidx)
            HP.KV sidx -> KeyValue InStaticTable (fromSIndexToIndex hdrtbl sidx)
        HP.K  hidx     -> KeyOnly  InHeaderTable (fromHIndexToIndex hdrtbl hidx)
        HP.KV hidx     -> KeyValue InHeaderTable (fromHIndexToIndex hdrtbl hidx)
  where
    mrev = reverseIndex hdrtbl

----------------------------------------------------------------

isIn :: Int -> HeaderTable -> Bool
isIn idx HeaderTable{..} = idx <= numOfEntries

-- | Which table does 'Index' belong to?
which :: HeaderTable -> Index -> IO (WhichTable, Entry)
which hdrtbl idx
  | idx `isIn` hdrtbl  = (InHeaderTable,) <$> toHeaderEntry hdrtbl hidx
  | isSIndexValid sidx = return (InStaticTable, toStaticEntry sidx)
  | otherwise          = throwIO $ IndexOverrun idx
  where
    hidx = fromIndexToHIndex hdrtbl idx
    sidx = fromIndexToSIndex hdrtbl idx

toHeaderEntry :: HeaderTable -> HIndex -> IO Entry
toHeaderEntry HeaderTable{..} (HIndex hidx) = readArray circularTable hidx
