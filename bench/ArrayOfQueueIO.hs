{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

-- Haskell implementation of H2O's priority queue.
-- https://github.com/h2o/h2o/blob/master/lib/http2/scheduler.c

module ArrayOfQueueIO (
    PriorityQueue(..)
  , new
  , enqueue
  , dequeue
  , delete
  ) where

import Control.Monad (replicateM)
import Data.Array (Array, listArray, (!))
import Data.Bits (setBit, clearBit, shiftR)
import qualified Data.HashTable.IO as H
import Data.IORef
import Data.Word (Word64)
import Foreign.C.Types (CLLong(..))

import DoublyLinkedQueueIO (Queue, Node)
import qualified DoublyLinkedQueueIO as Q

----------------------------------------------------------------

type Key = Int
type Weight = Int
type Deficit = Int
type Index = Int

type HashTable k v = H.BasicHashTable k v

----------------------------------------------------------------

data PriorityQueue a = PriorityQueue {
    bitsRef   :: IORef Word64
  , offsetRef :: IORef Index
  , queues    :: Array Index (Queue (Key, Weight, a))
  , deficits  :: HashTable Key Deficit
  , nodes     :: HashTable Key (Node (Key, Weight, a))
  }

----------------------------------------------------------------

bitWidth :: Int
bitWidth = 64

relativeIndex :: Index -> Index -> Index
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
-- >>> firstBitSet 0
-- -1
firstBitSet :: Word64 -> Index
firstBitSet x = ffs x - 1
  where
    ffs = fromIntegral . c_ffs . fromIntegral

----------------------------------------------------------------

new :: IO (PriorityQueue a)
new = PriorityQueue <$> newIORef 0 <*> newIORef 0 <*> newQueues <*> H.new <*> H.new
  where
    newQueues = listArray (0, bitWidth - 1) <$> replicateM bitWidth Q.new

-- | Enqueuing an entry. PriorityQueue is updated.
enqueue :: Key -> Weight -> a -> PriorityQueue a -> IO ()
enqueue k w x PriorityQueue{..} = do
    md <- H.lookup deficits k
    !deficit <- case md of
        Nothing -> return 0
        Just d  -> do
            H.delete deficits k
            return d
    let (!idx,!deficit') = calcIdxAndDeficit deficit
    !offidx <- getOffIdx idx
    node <- push offidx (k,w,x)
    H.insert nodes k node
    H.insert deficits k deficit'
    updateBits idx
  where
    calcIdxAndDeficit deficit = total `divMod` deficitSteps
      where
        total = deficitTable ! w + deficit
    getOffIdx idx = relativeIndex idx <$> readIORef offsetRef
    push offidx kwx = Q.enqueue kwx (queues ! offidx)
    updateBits idx = modifyIORef' bitsRef $ flip setBit idx

-- | Dequeuing an entry. PriorityQueue is updated.
dequeue :: PriorityQueue a -> IO (Maybe (Key, Weight, a))
dequeue pq@PriorityQueue{..} = do
    !idx <- getIdx
    if idx == -1 then
        return Nothing
      else do
        !offidx <- getOffIdx idx
        updateOffset offidx
        queueIsEmpty <- checkEmpty offidx
        updateBits idx queueIsEmpty
        if queueIsEmpty then
            dequeue pq
          else do
            ent@(k,_,_) <- pop offidx
            H.delete nodes k
            return $ Just ent
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

-- It's hard to update bitsRef.
-- So, dequeue checks bitsRef carefully.
delete :: Key -> PriorityQueue a -> IO (Maybe a)
delete k PriorityQueue{..} = do
    mnode <- H.lookup nodes k
    H.delete deficits k
    case mnode of
        Nothing   -> return Nothing
        Just node -> do
            Q.delete node
            let (_,_,x) = Q.item node
            return $ Just x
