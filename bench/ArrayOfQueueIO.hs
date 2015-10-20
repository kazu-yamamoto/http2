{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

-- Haskell implementation of H2O's priority queue.
-- https://github.com/h2o/h2o/blob/master/lib/http2/scheduler.c

module ArrayOfQueueIO (
    Entry
  , newEntry
  , renewEntry
  , item
  , PriorityQueue(..)
  , new
  , enqueue
  , dequeue
  ) where

import Control.Monad (replicateM)
import Data.Array (Array, listArray, (!))
import Data.Bits (setBit, clearBit, shiftR)
import Data.IORef
import Data.Word (Word64)
import Foreign.C.Types (CLLong(..))

----------------------------------------------------------------

type Weight = Int

-- | Abstract data type of entries for priority queues.
data Entry a = Entry {
    item :: a -- ^ Extracting an item from an entry.
  , weight  :: {-# UNPACK #-} !Weight
  , deficit :: {-# UNPACK #-} !Int
  } deriving Show

newEntry :: a -> Weight -> Entry a
newEntry x w = Entry x w 0

-- | Changing the item of an entry.
renewEntry :: Entry a -> b -> Entry b
renewEntry ent x = ent { item = x }

----------------------------------------------------------------

data PriorityQueue a = PriorityQueue {
    bitsRef   :: IORef Word64
  , offsetRef :: IORef Int
  , anchors   :: Array Int (IQueue (Entry a))
  }

----------------------------------------------------------------

bitWidth :: Int
bitWidth = 64

relativeIndex :: Int -> Int -> Int
relativeIndex idx offset = (offset + idx) `mod` bitWidth

----------------------------------------------------------------

deficitSteps :: Int
deficitSteps = 65536

deficitList :: [Int]
deficitList = map calc idxs
  where
    idxs :: [Double]
    idxs = [1..256]
    calc w = round (65536 * 63 / w)

deficitTable :: Array Int Int
deficitTable = listArray (1,256) deficitList

----------------------------------------------------------------

-- https://en.wikipedia.org/wiki/Find_first_set
foreign import ccall unsafe "strings.h ffsll"
    c_ffs :: CLLong -> CLLong

-- | Finding first bit set. O(1)
--
-- >>> firstBitSet $ setBit 0 63
-- 63
-- >>> firstBitSet $ setBit 0 62
-- 62
-- >>> firstBitSet $ setBit 0 1
-- 1
-- >>> firstBitSet $ setBit 0 0
-- 0
firstBitSet :: Word64 -> Int
firstBitSet x = ffs x - 1
  where
    ffs = fromIntegral . c_ffs . fromIntegral

----------------------------------------------------------------

new :: IO (PriorityQueue a)
new = PriorityQueue <$> newIORef 0 <*> newIORef 0 <*> newAnchors
  where
    newAnchors = listArray (0, bitWidth - 1) <$> replicateM bitWidth newIQueue

-- | Enqueuing an entry. PriorityQueue is updated.
enqueue :: Entry a -> PriorityQueue a -> IO ()
enqueue ent PriorityQueue{..} = do
    let (!idx,!deficit') = calcIdxAndDeficit
    !offidx <- getOffIdx idx
    push offidx ent { deficit = deficit' }
    updateBits idx
  where
    calcIdxAndDeficit = total `divMod` deficitSteps
      where
        total = deficitTable ! weight ent + deficit ent
    getOffIdx idx = relativeIndex idx <$> readIORef offsetRef
    push offidx ent' = writeIQueue (anchors ! offidx) ent'
    updateBits idx = modifyIORef' bitsRef $ flip setBit idx

-- | Dequeuing an entry. PriorityQueue is updated.
dequeue :: PriorityQueue a -> IO (Entry a)
dequeue PriorityQueue{..} = do
    !idx <- getIdx
    !offidx <- getOffIdx idx
    !ent <- pop offidx
    updateOffset offidx
    checkEmpty offidx >>= updateBits idx
    return ent
  where
    getIdx = firstBitSet <$> readIORef bitsRef
    getOffIdx idx = relativeIndex idx <$> readIORef offsetRef
    pop offidx = readIQueue (anchors ! offidx)
    checkEmpty offidx = isEmptyIQueue (anchors ! offidx)
    updateOffset offset' = writeIORef offsetRef offset'
    updateBits idx isEmpty = modifyIORef' bitsRef shiftClear
      where
        shiftClear bits
          | isEmpty   = clearBit (shiftR bits idx) 0
          | otherwise = shiftR bits idx

----------------------------------------------------------------

data IQueue a = IQueue {-# UNPACK #-} !(IORef [a])
                       {-# UNPACK #-} !(IORef [a])


newIQueue :: IO (IQueue a)
newIQueue = IQueue <$> newIORef [] <*> newIORef []

writeIQueue :: IQueue a -> a -> IO ()
writeIQueue (IQueue _read write) a = do
  listend <- readIORef write
  writeIORef write (a:listend)

readIQueue :: IQueue a -> IO a
readIQueue (IQueue read write) = do
  xs <- readIORef read
  case xs of
    (x:xs') -> do writeIORef read xs'
                  return x
    [] -> do ys <- readIORef write
             case ys of
               [] -> error "readIQueue"
               _  -> case reverse ys of
                       [] -> error "readIQueue"
                       (z:zs) -> do writeIORef write []
                                    writeIORef read zs
                                    return z

isEmptyIQueue :: IQueue a -> IO Bool
isEmptyIQueue (IQueue read write) = do
  xs <- readIORef read
  case xs of
    (_:_) -> return False
    [] -> do ys <- readIORef write
             case ys of
               [] -> return True
               _  -> return False
