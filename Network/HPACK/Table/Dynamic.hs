{-# LANGUAGE TupleSections, RecordWildCards, FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Network.HPACK.Table.Dynamic (
    DynamicTable(..)
  , newDynamicTableForEncoding
  , newDynamicTableForDecoding
  , renewDynamicTable
  , huffmanDecoder
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
  , CodeInfo(..)
  ) where

import Control.Monad (forM, when)
import Data.Array.IO (IOArray, newArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Network.HPACK.Huffman
import Network.HPACK.Table.Entry
import Network.HPACK.Table.RevIndex
import Network.HPACK.Table.Static
import Foreign.Marshal.Alloc

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

data CodeInfo =
    EncodeInfo !(IORef Outer) -- Reverse index
               -- The value informed by SETTINGS_HEADER_TABLE_SIZE.
               -- If 'Nothing', dynamic table size update is not necessary.
               -- Otherwise, dynamic table size update is sent
               -- and this value should be set to 'Nothing'.
               !(IORef (Maybe Size))
  | DecodeInfo !HuffmanDecoding
               !(IORef Size)  -- The limit size

-- | Type for dynamic table.
data DynamicTable = DynamicTable {
    codeInfo :: !CodeInfo
  -- | An array
  , circularTable :: !(IORef Table)
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
  }

{-# INLINE adj #-}
adj :: Int -> Int -> Int
adj maxN x = (x + maxN) `mod` maxN

huffmanDecoder :: DynamicTable -> HuffmanDecoding
huffmanDecoder DynamicTable{..} = dec
  where
    DecodeInfo dec _ = codeInfo

----------------------------------------------------------------

-- | Printing 'DynamicTable'.
printDynamicTable :: DynamicTable -> IO ()
printDynamicTable DynamicTable{..} = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    n <- readIORef numOfEntries
    let !beg = off + 1
        !end = off + n
    tbl <- readIORef circularTable
    es <- mapM (readArray tbl . adj maxN) [beg .. end]
    let !ts = zip [1..] es
    mapM_ printEntry ts
    dsize <- readIORef dynamicTableSize
    maxdsize <- readIORef maxDynamicTableSize
    putStrLn $ "      Table size: " ++ show dsize ++ "/" ++ show maxdsize
    case codeInfo of
        EncodeInfo revref _ -> readIORef revref >>= print
        _                   -> return ()

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
    let DecodeInfo _ limref = codeInfo
    lim <- readIORef limref
    return $! siz <= lim && maxNumbers siz /= 0

data TableSizeAction = Keep | Change !Size | Ignore !Size

needChangeTableSize :: DynamicTable -> IO TableSizeAction
needChangeTableSize DynamicTable{..} = do
    let EncodeInfo _ limref = codeInfo
    mlim <- readIORef limref
    maxsiz <- readIORef maxDynamicTableSize
    return $ case mlim of
        Nothing          -> Keep
        Just lim
          | lim < maxsiz -> Change lim
          | otherwise    -> Ignore maxsiz

-- | When SETTINGS_HEADER_TABLE_SIZE is received from a peer,
--   its value should be set by this function.
setLimitForEncoding :: Size -> DynamicTable -> IO ()
setLimitForEncoding siz DynamicTable{..} = do
    let EncodeInfo _ limref = codeInfo
    writeIORef limref $! Just siz

resetLimitForEncoding :: DynamicTable -> IO ()
resetLimitForEncoding DynamicTable{..} = do
    let EncodeInfo _ limref = codeInfo
    writeIORef limref Nothing

----------------------------------------------------------------

-- | Creating 'DynamicTable'.
newDynamicTableForEncoding :: Size -- ^ The dynamic table size
                           -> IO DynamicTable
newDynamicTableForEncoding maxsiz = do
    rev <- newIORef defaultRevIndex
    lim <- newIORef Nothing
    let !info = EncodeInfo rev lim
    newDynamicTable maxsiz info

-- | Creating 'DynamicTable'.
newDynamicTableForDecoding :: Size -- ^ The dynamic table size
                           -> Size -- ^ The size of temporary buffer for Huffman decoding
                           -> IO DynamicTable
newDynamicTableForDecoding maxsiz huftmpsiz = do
    lim <- newIORef maxsiz
    buf <- mallocBytes huftmpsiz
    let !decoder = decode buf huftmpsiz
        !info = DecodeInfo decoder lim
    newDynamicTable maxsiz info

newDynamicTable :: Size -> CodeInfo -> IO DynamicTable
newDynamicTable maxsiz info = do
    tbl <- newArray (0,end) dummyEntry
    DynamicTable info <$> newIORef tbl     -- circularTable
                      <*> newIORef end     -- offset
                      <*> newIORef 0       -- numOfEntries
                      <*> newIORef maxN    -- maxNumOfEntries
                      <*> newIORef 0       -- dynamicTableSize
                      <*> newIORef maxsiz  -- maxDynamicTableSize
  where
    !maxN = maxNumbers maxsiz
    !end = maxN - 1

-- | Renewing 'DynamicTable' with necessary entries copied.
renewDynamicTable :: Size -> DynamicTable -> IO ()
renewDynamicTable maxsiz dyntbl@DynamicTable{..} = do
    renew <- shouldRenew dyntbl maxsiz
    when renew $ do
        !entries <- getEntries dyntbl
        let !maxN = maxNumbers maxsiz
            !end = maxN - 1
        newtbl <- newArray (0,end) dummyEntry
        writeIORef circularTable newtbl
        writeIORef offset end
        writeIORef numOfEntries 0
        writeIORef maxNumOfEntries maxN
        writeIORef dynamicTableSize 0
        writeIORef maxDynamicTableSize maxsiz
        case codeInfo of
            EncodeInfo revref _ -> writeIORef revref defaultRevIndex
            _                   -> return ()
        copyEntries dyntbl entries

getEntries :: DynamicTable -> IO [Entry]
getEntries DynamicTable{..} = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    n <- readIORef numOfEntries
    table <- readIORef circularTable
    let readTable i = readArray table $ adj maxN (off + i)
    forM [1 .. n] readTable

copyEntries :: DynamicTable -> [Entry] -> IO ()
copyEntries _                           [] = return ()
copyEntries dyntbl@DynamicTable{..} (e:es) = do
    dsize <- readIORef dynamicTableSize
    maxdsize <- readIORef maxDynamicTableSize
    when (dsize + entrySize e <= maxdsize) $ do
        insertEnd e dyntbl
        copyEntries dyntbl es

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
    case codeInfo of
        EncodeInfo revref _ -> do
            rev <- readIORef revref
            writeIORef revref $! deleteDynamicList hs rev
        _                   -> return ()

insertFront :: Entry -> DynamicTable -> IO ()
insertFront e DynamicTable{..} = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    n <- readIORef numOfEntries
    dsize <- readIORef dynamicTableSize
    table <- readIORef circularTable
    let i = off
        !dsize' = dsize + entrySize e
        !off' = adj maxN (off - 1)
    writeArray table i e
    writeIORef offset off'
    writeIORef numOfEntries $ n + 1
    writeIORef dynamicTableSize dsize'
    case codeInfo of
        EncodeInfo revref _ -> do
            rev <- readIORef revref
            writeIORef revref $! insertDynamic (entryHeader e) (DIndex i) rev
        _                   -> return ()

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
    table <- readIORef circularTable
    let !i = adj maxN (off + n + 1)
        !dsize' = dsize + entrySize e
    writeArray table i e
    writeIORef numOfEntries $ n + 1
    writeIORef dynamicTableSize dsize'
    case codeInfo of
        EncodeInfo revref _ -> do
            rev <- readIORef revref
            writeIORef revref $! insertDynamic (entryHeader e) (DIndex i) rev
        _                   -> return ()

----------------------------------------------------------------

removeEnd :: DynamicTable -> IO Header
removeEnd DynamicTable{..} = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    n <- readIORef numOfEntries
    let !i = adj maxN (off + n)
    table <- readIORef circularTable
    e <- readArray table i
    writeArray table i dummyEntry -- let the entry GCed
    dsize <- readIORef dynamicTableSize
    let !dsize' = dsize - entrySize e
        !h = entryHeader e
    writeIORef numOfEntries (n - 1)
    writeIORef dynamicTableSize dsize'
    return h

----------------------------------------------------------------

toHeaderEntry :: DynamicTable -> DIndex -> IO Entry
toHeaderEntry DynamicTable{..} (DIndex didx) = do
    table <- readIORef circularTable
    readArray table didx
