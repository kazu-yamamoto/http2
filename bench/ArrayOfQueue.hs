{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

-- Haskell implementation of H2O's priority queue.
-- https://github.com/h2o/h2o/blob/master/lib/http2/scheduler.c

module ArrayOfQueue (
    PriorityQueue(..)
  , new
  , enqueue
  , dequeue
  ) where

import Control.Concurrent.STM
import Control.Monad (replicateM)
import Data.Array (Array, listArray, (!))
import Data.Bits (setBit, clearBit, shiftR)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
import Data.Word (Word64)
import Foreign.C.Types (CLLong(..))

----------------------------------------------------------------

type Key = Int
type Weight = Int
type Deficit = Int
type Index = Int

----------------------------------------------------------------

data PriorityQueue a = PriorityQueue {
    bitsRef   :: TVar Word64
  , offsetRef :: TVar Index
  , deficits  :: TVar (IntMap Deficit)
  , queues    :: Array Index (TQueue (Key, Weight, a))
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

new :: STM (PriorityQueue a)
new = PriorityQueue <$> newTVar 0 <*> newTVar 0 <*> newTVar I.empty <*> newQueues
  where
    newQueues = listArray (0, bitWidth - 1) <$> replicateM bitWidth newTQueue

-- | Enqueuing an entry. PriorityQueue is updated.
enqueue :: Key -> Weight -> a -> PriorityQueue a -> STM ()
enqueue k w x PriorityQueue{..} = do
    md <- lookupDeficit
    let !deficit = maybe 0 id md
        (!idx,!deficit') = calcIdxAndDeficit deficit
    !offidx <- getOffIdx idx
    push offidx (k,w,x)
    updateDeficits deficit'
    updateBits idx
  where
    calcIdxAndDeficit deficit = total `divMod` deficitSteps
      where
        total = deficitTable ! w + deficit
    getOffIdx idx = relativeIndex idx <$> readTVar offsetRef
    push offidx kwx = writeTQueue (queues ! offidx) kwx
    updateBits idx = modifyTVar' bitsRef $ flip setBit idx
    lookupDeficit = I.lookup k <$> readTVar deficits
    updateDeficits d = modifyTVar' deficits $ I.insert k d

-- | Dequeuing an entry. PriorityQueue is updated.
dequeue :: PriorityQueue a -> STM (Key, Weight, a)
dequeue PriorityQueue{..} = do
    !idx <- getIdx
    !offidx <- getOffIdx idx
    ent <- pop offidx
    updateOffset offidx
    checkEmpty offidx >>= updateBits idx
    return ent
  where
    getIdx = firstBitSet <$> readTVar bitsRef
    getOffIdx idx = relativeIndex idx <$> readTVar offsetRef
    pop offidx = readTQueue (queues ! offidx)
    checkEmpty offidx = isEmptyTQueue (queues ! offidx)
    updateOffset offset' = writeTVar offsetRef offset'
    updateBits idx isEmpty = modifyTVar' bitsRef shiftClear
      where
        shiftClear bits
          | isEmpty   = clearBit (shiftR bits idx) 0
          | otherwise = shiftR bits idx
