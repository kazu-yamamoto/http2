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

module Network.HTTP2.RandomSkewHeap (
    Heap
  , empty
  , isEmpty
  , singleton
  , insert
  , uncons
  ) where

import Network.HTTP2.Types (Weight)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC (createSystemRandom, uniformR, GenIO)

data Heap a = Leaf | Node Weight -- total
                          a Weight !(Heap a) !(Heap a) deriving Show

empty :: Heap a
empty = Leaf

isEmpty :: Heap a -> Bool
isEmpty Leaf = True
isEmpty _    = False

singleton :: a -> Weight -> Heap a
singleton a w = Node w a w Leaf Leaf

insert :: a -> Weight -> Heap a -> Heap a
insert a w t = merge (singleton a w) t

merge :: Heap t -> Heap t -> Heap t
merge t Leaf = t
merge Leaf t = t
merge l@(Node tw1 a1 w1 l1 r1) r@(Node tw2 a2 w2 l2 r2)
  | g < w1    = Node tw a1 w1 r1 $ merge l1 r
  | otherwise = Node tw a2 w2 r2 $ merge l2 l
  where
    tw = tw1 + tw2
    g = unsafePerformIO $ uniformR (1,tw) gen
{-# NOINLINE merge #-}

uncons :: Heap a -> Maybe (a, Weight, Heap a)
uncons Leaf             = Nothing
uncons (Node _ a w l r) = Just (a, w, t)
  where
    !t = merge l r

gen :: GenIO
gen = unsafePerformIO createSystemRandom

