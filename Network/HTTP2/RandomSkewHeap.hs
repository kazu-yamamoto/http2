{-# LANGUAGE BangPatterns #-}

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
import System.Random.Mersenne (randomIO)

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
    g = unsafePerformIO randomIO `mod` tw

uncons :: Heap a -> Maybe (a, Weight, Heap a)
uncons Leaf             = Nothing
uncons (Node _ a w l r) = Just (a, w, t)
  where
    !t = merge l r
