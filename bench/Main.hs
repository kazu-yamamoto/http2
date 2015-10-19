{-# LANGUAGE BangPatterns #-}

module Main where

import qualified ArrayOfQueue as A
import Control.Concurrent.STM
import Criterion.Main
import qualified Network.HTTP2.Priority.Heap as H
import qualified RandomSkewHeap as R
import System.Random

main :: IO ()
main = do
    gen <- getStdGen
    let xs = take 100 $ randomRs (1,256) gen
    defaultMain [
        bench "Heap"           $ whnf bench1 xs
      , bench "RandomSkewHeap" $ whnf bench2 xs
      , bench "Array"          $ nfIO (bench3 xs)
      ]

bench1 :: [Int] -> ()
bench1 xs = enqdeq1 pq 10000
  where
    !pq = create1 xs H.empty

create1 :: [Int] -> H.PriorityQueue Int -> H.PriorityQueue Int
create1 [] !q = q
create1 (x:xs) !q = create1 xs q'
  where
    !ent = H.newEntry x x
    !q' = H.enqueue ent q

enqdeq1 :: H.PriorityQueue Int -> Int -> ()
enqdeq1 _ 0  = ()
enqdeq1 q !n = case H.dequeue q of
    Nothing -> error "enqdeq1"
    Just (ent,q') -> let q'' = H.enqueue ent q'
                     in enqdeq1 q'' (n - 1)

bench2 :: [Int] -> ()
bench2 xs = enqdeq2 pq 10000
  where
    !pq = create2 xs R.empty

create2 :: [Int] -> R.PriorityQueue Int -> R.PriorityQueue Int
create2 [] !q = q
create2 (x:xs) !q = create2 xs q'
  where
    !ent = R.newEntry x x
    !q' = R.enqueue ent q

enqdeq2 :: R.PriorityQueue Int -> Int -> ()
enqdeq2 _ 0  = ()
enqdeq2 q !n = case R.dequeue q of
    Nothing -> error "enqdeq2"
    Just (ent,q') -> let q'' = R.enqueue ent q'
                     in enqdeq2 q'' (n - 1)

bench3 :: [Int] -> IO ()
bench3 xs = do
    q <- atomically A.new
    create3 xs q
    enqdeq3 q 10000

create3 :: [Int] -> A.PriorityQueue Int -> IO ()
create3 [] _      = return ()
create3 (x:xs) !q = do
    let !ent = A.newEntry x x
    atomically $ A.enqueue ent q
    create3 xs q

enqdeq3 :: A.PriorityQueue Int -> Int -> IO ()
enqdeq3 _ 0  = return ()
enqdeq3 q !n = do
    ent <- atomically $ A.dequeue q
    atomically $ A.enqueue ent q
    enqdeq3 q (n - 1)
