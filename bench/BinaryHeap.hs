{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module BinaryHeap (
    Entry
  , newEntry
  , renewEntry
  , item
  , PriorityQueue(..)
  , new
  , enqueue
  , dequeue
  ) where

import Control.Concurrent.STM
import Control.Monad (when)
import Data.Array (Array, listArray, (!))
import Data.Array.MArray (newArray_, readArray, writeArray)
import Data.Word (Word64)

----------------------------------------------------------------

type Weight = Int
type Deficit = Word64

-- | Abstract data type of entries for priority queues.
data Entry a = Entry {
    item :: a -- ^ Extracting an item from an entry.
  , weight  :: {-# UNPACK #-} !Weight
  , deficit :: {-# UNPACK #-} !Deficit
  } deriving Show

instance Eq (Entry a) where
    Entry _ _ p1 == Entry _ _ p2 = p1 == p2

instance Ord (Entry a) where
    Entry _ _ p1 <  Entry _ _ p2 = p1 <  p2
    Entry _ _ p1 <= Entry _ _ p2 = p1 <= p2

newEntry :: a -> Weight -> Entry a
newEntry x w = Entry x w 0

-- | Changing the item of an entry.
renewEntry :: Entry a -> b -> Entry b
renewEntry ent x = ent { item = x }

----------------------------------------------------------------

type Index = Int
type TA a = TArray Index (Entry a)

-- FIXME: The base (Word64) would be overflowed.
--        In that case, the heap must be re-constructed.
data PriorityQueue a = PriorityQueue (TVar Deficit)
                                     (TVar Index)
                                     (TA a)

----------------------------------------------------------------

magicDeficit :: Deficit
magicDeficit = 0

deficitSteps :: Int
deficitSteps = 65536

deficitList :: [Deficit]
deficitList = map calc idxs
  where
    idxs = [1..256] :: [Double]
    calc w = round (fromIntegral deficitSteps / w)

deficitTable :: Array Index Deficit
deficitTable = listArray (1,256) deficitList

weightToDeficit :: Weight -> Deficit
weightToDeficit w = deficitTable ! w

----------------------------------------------------------------

new :: Int -> STM (PriorityQueue a)
new n = PriorityQueue <$> newTVar 0 <*> newTVar 1 <*> newArray_ (1,n)

-- | Enqueuing an entry. PriorityQueue is updated.
enqueue :: Entry a -> PriorityQueue a -> STM ()
enqueue Entry{..} (PriorityQueue bvar idx arr) = do
    i <- readTVar idx
    base <- readTVar bvar
    let !b = if deficit == magicDeficit then base else deficit
        !deficit' = b + weightToDeficit weight
        !ent' = Entry item weight deficit'
    writeArray arr i ent'
    shiftUp arr i
    let i' = i + 1
    writeTVar idx i'
    return ()

-- | Dequeuing an entry. PriorityQueue is updated.
dequeue :: PriorityQueue a -> STM (Entry a)
dequeue (PriorityQueue bvar idx arr) = do
    ent <- readArray arr 1
    i <- readTVar idx
    -- fixme: checking if i == 0
    let i' = i - 1
    readArray arr i' >>= writeArray arr 1
    shiftDown arr 1 i'
    writeTVar idx i'
    writeTVar bvar $ if i' == 1 then 0 else deficit ent
    return ent

{-# INLINE shiftUp #-}
shiftUp :: TA a -> Int -> STM ()
shiftUp _ 1 = return ()
shiftUp arr c = swapAndDo arr p c (shiftUp arr p)
  where
    p = c `div` 2

{-# INLINE shiftDown #-}
shiftDown :: TA a -> Int -> Int -> STM ()
shiftDown arr p n
  | c1 > n    = return ()
  | c1 == n   = swapAndDo arr p c1 (return ())
  | otherwise = do
      let c2 = c1 + 1
      xc1 <- readArray arr c1
      xc2 <- readArray arr c2
      let c = if xc1 > xc2 then c1 else c2
      swapAndDo arr p c (shiftDown arr c n)
  where
    c1 = 2 * p

{-# LANGUAGE swapAndDo #-}
swapAndDo :: TA a -> Int -> Int -> STM () -> STM ()
swapAndDo arr p c cont = do
    xp <- readArray arr p
    xc <- readArray arr c
    when (xc > xp) $ do
        writeArray arr c xp
        writeArray arr p xc
        cont
