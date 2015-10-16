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

module Network.HTTP2.Priority.RandomSkewHeap (
    Entry(..)
  , newEntry
  , renewEntry
  , item
  , PriorityQueue
  , empty
  , isEmpty
  , enqueue
  , dequeue
  ) where

import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC (createSystemRandom, uniformR, GenIO)

----------------------------------------------------------------

type Weight = Int

data Entry a = Entry a {-# UNPACK #-} !Weight deriving Show

----------------------------------------------------------------

newEntry :: a -> Weight -> Entry a
newEntry x w = Entry x w

renewEntry :: Entry a -> b -> Entry b
renewEntry (Entry _ w) x = Entry x w

item :: Entry a -> a
item (Entry x _) = x

----------------------------------------------------------------

data PriorityQueue a = Leaf | Node {-# UNPACK #-} !Weight -- total
                                   (Entry a)
                                   !(PriorityQueue a)
                                   !(PriorityQueue a) deriving Show

----------------------------------------------------------------

empty :: PriorityQueue a
empty = Leaf

isEmpty :: PriorityQueue a -> Bool
isEmpty Leaf = True
isEmpty _    = False

singleton :: Entry a -> PriorityQueue a
singleton ent@(Entry _ w) = Node w ent Leaf Leaf

----------------------------------------------------------------

enqueue :: Entry a -> PriorityQueue a -> PriorityQueue a
enqueue ent q = merge (singleton ent) q

-- if l is a singleton, w1 == tw1.
merge :: PriorityQueue t -> PriorityQueue t -> PriorityQueue t
merge t Leaf = t
merge Leaf t = t
merge l@(Node tw1 ent1 ll lr) r@(Node tw2 ent2 rl rr)
  | g <= tw1  = Node tw ent1 lr $ merge ll r
  | otherwise = Node tw ent2 rr $ merge rl l
  where
    tw = tw1 + tw2
    g = unsafePerformIO $ uniformR (1,tw) gen
{-# NOINLINE merge #-}

dequeue :: PriorityQueue a -> Maybe (Entry a, PriorityQueue a)
dequeue Leaf             = Nothing
dequeue (Node _ ent l r) = Just (ent, t)
  where
    !t = merge l r

gen :: GenIO
gen = unsafePerformIO createSystemRandom
