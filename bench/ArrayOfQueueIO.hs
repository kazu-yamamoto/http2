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
  , Node
  , PriorityQueue(..)
  , new
  , enqueue
  , dequeue
  , delete
  ) where

import Control.Monad (replicateM)
import Data.Array (Array, listArray, (!))
import Data.Bits (setBit, clearBit, shiftR)
import Data.IORef
import Data.Word (Word64)
import Foreign.C.Types (CLLong(..))

import DoublyLinkedQueueIO (Queue, Node)
import qualified DoublyLinkedQueueIO as Q

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
  , queues    :: Array Int (Queue (Entry a))
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
new = PriorityQueue <$> newIORef 0 <*> newIORef 0 <*> newQueues
  where
    newQueues = listArray (0, bitWidth - 1) <$> replicateM bitWidth Q.new

-- | Enqueuing an entry. PriorityQueue is updated.
enqueue :: Entry a -> PriorityQueue a -> IO (Node (Entry a))
enqueue ent PriorityQueue{..} = do
    let (!idx,!deficit') = calcIdxAndDeficit
    !offidx <- getOffIdx idx
    node <- push offidx ent { deficit = deficit' }
    updateBits idx
    return node
  where
    calcIdxAndDeficit = total `divMod` deficitSteps
      where
        total = deficitTable ! weight ent + deficit ent
    getOffIdx idx = relativeIndex idx <$> readIORef offsetRef
    push offidx ent' = Q.enqueue ent' (queues ! offidx)
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
    pop offidx = Q.dequeue (queues ! offidx)
    checkEmpty offidx = Q.isEmpty (queues ! offidx)
    updateOffset offset' = writeIORef offsetRef offset'
    updateBits idx isEmpty = modifyIORef' bitsRef shiftClear
      where
        shiftClear bits
          | isEmpty   = clearBit (shiftR bits idx) 0
          | otherwise = shiftR bits idx

-- bits is not updated because it's difficult.
delete :: Node (Entry a) -> IO ()
delete node = Q.delete node
