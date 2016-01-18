{-# LANGUAGE BangPatterns #-}

-- This data structure is based on skew heap.
--
-- If we take weight as priority, a typical heap (priority queue)
-- is not fair enough. Consider two weight 201 for A and 101 for B.
-- A typical heap would generate A(201), A(200), A(199), A(198), ....,
-- and finaly A(101), B(101), A(100), B(100).
-- What we want is A, A, B, A, A, B...
--
-- So, we introduce randomness to Skew Heap.
--
-- In the random binary tree,
-- an element is selected as the root with probability of
-- 1 / (n + 1) where n is the size of the original tree.
-- In the random skew heap, an element is selected as the root
-- with the probability of weight / total_weight.
--
-- Since this data structure uses random numbers, APIs should be
-- essentially impure. But since this is used with STM,
-- APIs are made to be pure with unsafePerformIO.

module RandomSkewHeap (
    PriorityQueue
  , empty
  , isEmpty
  , enqueue
  , dequeue
  , delete
  ) where

import Data.List (partition)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC (createSystemRandom, uniformR, GenIO)

----------------------------------------------------------------

type Key = Int
type Weight = Int

----------------------------------------------------------------

data PriorityQueue a = Leaf | Node {-# UNPACK #-} !Weight -- total
                                   {-# UNPACK #-} !Key
                                   {-# UNPACK #-} !Weight
                                   !a
                                   !(PriorityQueue a)
                                   !(PriorityQueue a) deriving Show

----------------------------------------------------------------

empty :: PriorityQueue a
empty = Leaf

isEmpty :: PriorityQueue a -> Bool
isEmpty Leaf = True
isEmpty _    = False

singleton :: Key -> Weight -> a -> PriorityQueue a
singleton k w v = Node w k w v Leaf Leaf

----------------------------------------------------------------

enqueue :: Key -> Weight -> a -> PriorityQueue a -> PriorityQueue a
enqueue k w v q = merge (singleton k w v) q

-- if l is a singleton, w1 == tw1.
merge :: PriorityQueue t -> PriorityQueue t -> PriorityQueue t
merge t Leaf = t
merge Leaf t = t
merge l@(Node tw1 k1 w1 v1 ll lr) r@(Node tw2 k2 w2 v2 rl rr)
  | g <= w1   = Node tw k1 w1 v1 lr $ merge ll r
  | otherwise = Node tw k2 w2 v2 rr $ merge rl l
  where
    tw = tw1 + tw2
    g = unsafePerformIO $ uniformR (1,tw) gen
{-# NOINLINE merge #-}

dequeue :: PriorityQueue a -> Maybe (Key, Weight, a, PriorityQueue a)
dequeue Leaf               = Nothing
dequeue (Node _ k w v l r) = Just (k, w, v, t)
  where
    !t = merge l r

delete :: Key -> PriorityQueue a -> (Maybe a, PriorityQueue a)
delete k q = (mv, fromList xs')
  where
    !xs = toList q
    (!ds,!xs') = partition (\(k',_,_) -> k' == k) xs
    mv = case ds of
       []          -> Nothing
       ((_,_,v):_) -> Just v

toList :: PriorityQueue a -> [(Key, Weight, a)]
toList q = go q id []
  where
    go Leaf b = b
    go (Node _ k w v l r) b = go r (go l (((k,w,v) :) . b))

fromList :: [(Key, Weight, a)] -> PriorityQueue a
fromList xs = go empty xs
  where
    go !q [] = q
    go !q ((k,w,v):xks) = go (enqueue k w v q) xks

gen :: GenIO
gen = unsafePerformIO createSystemRandom
