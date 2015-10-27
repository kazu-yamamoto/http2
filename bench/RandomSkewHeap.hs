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
                                   (Key, Weight, a)
                                   !(PriorityQueue a)
                                   !(PriorityQueue a) deriving Show

----------------------------------------------------------------

empty :: PriorityQueue a
empty = Leaf

isEmpty :: PriorityQueue a -> Bool
isEmpty Leaf = True
isEmpty _    = False

singleton :: Key -> Weight -> a -> PriorityQueue a
singleton k w x = Node w (k,w,x) Leaf Leaf

----------------------------------------------------------------

enqueue :: Key -> Weight -> a -> PriorityQueue a -> PriorityQueue a
enqueue k w x q = merge (singleton k w x) q

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

dequeue :: PriorityQueue a -> Maybe (Key, Weight, a, PriorityQueue a)
dequeue Leaf                 = Nothing
dequeue (Node _ (k,w,x) l r) = Just (k, w, x, t)
  where
    !t = merge l r

delete :: Key -> PriorityQueue a -> (Maybe a, PriorityQueue a)
delete k q = (r, q')
  where
    !xs = toList q
    (ds, !ys) = partition (\(k',_,_) -> k' == k) xs
    !q' = fromList ys
    !r = case ds of
        []          -> Nothing
        ((_,_,x):_) -> Just x

toList :: PriorityQueue a -> [(Key,Weight,a)]
toList q = go q id []
  where
    go Leaf b             = b
    go (Node _ ent l r) b = go r (go l ((ent :) . b))

fromList :: [(Key,Weight,a)] -> PriorityQueue a
fromList xs = go empty xs
  where
    go !q [] = q
    go !q ((k,w,x):kwxs) = go (enqueue k w x q) kwxs

gen :: GenIO
gen = unsafePerformIO createSystemRandom
