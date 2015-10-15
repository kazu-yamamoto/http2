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
    PriorityQueue
  , empty
  , isEmpty
  , enqueue
  , dequeue
  ) where

import Network.HTTP2.Types (Weight)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC (createSystemRandom, uniformR, GenIO)

data PriorityQueue a = Leaf | Node Weight -- total
                                   a Weight !(PriorityQueue a) !(PriorityQueue a) deriving Show

empty :: PriorityQueue a
empty = Leaf

isEmpty :: PriorityQueue a -> Bool
isEmpty Leaf = True
isEmpty _    = False

singleton :: a -> Weight -> PriorityQueue a
singleton a w = Node w a w Leaf Leaf

enqueue :: a -> Weight -> PriorityQueue a -> PriorityQueue a
enqueue a w t = merge (singleton a w) t

-- if l is a singleton, w1 == tw1.
merge :: PriorityQueue t -> PriorityQueue t -> PriorityQueue t
merge t Leaf = t
merge Leaf t = t
merge l@(Node tw1 x1 w1 ll lr) r@(Node tw2 x2 w2 rl rr)
  | g <= tw1  = Node tw x1 w1 lr $ merge ll r
  | otherwise = Node tw x2 w2 rr $ merge rl l
  where
    tw = tw1 + tw2
    g = unsafePerformIO $ uniformR (1,tw) gen
{-# NOINLINE merge #-}

dequeue :: PriorityQueue a -> Maybe (a, Weight, PriorityQueue a)
dequeue Leaf             = Nothing
dequeue (Node _ a w l r) = Just (a, w, t)
  where
    !t = merge l r

gen :: GenIO
gen = unsafePerformIO createSystemRandom
{-# NOINLINE gen #-}

{-
main :: IO ()
main = do
    let q = enqueue "c" 1 $ enqueue "b" 101 $ enqueue "a" 201 empty
    loop 1000 q
  where
    loop :: Int -> PriorityQueue String -> IO ()
    loop 0 _ = return ()
    loop n q = do
        case dequeue q of
            Nothing -> error "Nothing"
            Just (x, w, q') -> do
                putStrLn x
                loop (n-1) (enqueue x w q')
-}
