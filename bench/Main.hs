{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent.STM
import Criterion.Main
import System.Random

import qualified ArrayOfQueue as A
--import qualified BinaryHeap as B
import qualified Network.HTTP2.Priority.Heap as O
import qualified PSQ as P
import qualified RandomSkewHeap as R

numOfStreams :: Int
numOfStreams = 100

numOfTrials :: Int
numOfTrials = 10000

main :: IO ()
main = do
    gen <- getStdGen
    let xs = take numOfStreams $ randomRs (1,256) gen
        ss = [1,3..]
        ys = zip ss xs
    defaultMain [
        bench "RandomSkewHeap" $ whnf benchR xs
      , bench "Okasaki Heap"   $ whnf benchO xs
      , bench "PSQ"            $ whnf benchP ys
--      , bench "Binary Heap"    $ nfIO (benchB xs)
      , bench "Array of Queue" $ nfIO (benchA xs)
      ]

----------------------------------------------------------------

benchR :: [Int] -> ()
benchR ys = enqdeqR pq numOfTrials
  where
    !pq = createR ys R.empty

createR :: [Int] -> R.PriorityQueue Int -> R.PriorityQueue Int
createR [] !q = q
createR (x:xs) !q = createR xs q'
  where
    !ent = R.newEntry x x
    !q' = R.enqueue ent q

enqdeqR :: R.PriorityQueue Int -> Int -> ()
enqdeqR _ 0  = ()
enqdeqR q !n = case R.dequeue q of
    Nothing -> error "enqdeqR"
    Just (ent,q') -> let !q'' = R.enqueue ent q'
                       in enqdeqR q'' (n - 1)

----------------------------------------------------------------

benchO :: [Int] -> ()
benchO xs = enqdeqO pq numOfTrials
  where
    !pq = createO xs O.empty

createO :: [Int] -> O.PriorityQueue Int -> O.PriorityQueue Int
createO [] !q = q
createO (x:xs) !q = createO xs q'
  where
    !ent = O.newEntry x x
    !q' = O.enqueue ent q

enqdeqO :: O.PriorityQueue Int -> Int -> ()
enqdeqO _ 0  = ()
enqdeqO q !n = case O.dequeue q of
    Nothing -> error "enqdeqO"
    Just (ent,q') -> let !q'' = O.enqueue ent q'
                     in enqdeqO q'' (n - 1)

----------------------------------------------------------------

benchP :: [(Int,Int)] -> ()
benchP xs = enqdeqP pq numOfTrials
  where
    !pq = createP xs P.empty

createP :: [(Int,Int)] -> P.PriorityQueue Int -> P.PriorityQueue Int
createP [] !q = q
createP ((k,x):xs) !q = createP xs q'
  where
    !ent = P.newEntry x x
    !q' = P.enqueue k ent q

enqdeqP :: P.PriorityQueue Int -> Int -> ()
enqdeqP _ 0  = ()
enqdeqP q !n = case P.dequeue q of
    Nothing -> error "enqdeqP"
    Just (k,ent,q') -> let !q'' = P.enqueue k ent q'
                       in enqdeqP q'' (n - 1)

----------------------------------------------------------------

{-
benchB :: [Int] -> IO ()
benchB xs = do
    q <- atomically (B.new numOfStreams)
    createB xs q
    enqdeqB q numOfTrials

createB :: [Int] -> B.PriorityQueue Int -> IO ()
createB [] _      = return ()
createB (x:xs) !q = do
    let !ent = B.newEntry x x
    atomically $ B.enqueue ent q
    createB xs q

enqdeqB :: B.PriorityQueue Int -> Int -> IO ()
enqdeqB _ 0  = return ()
enqdeqB q !n = do
    ent <- atomically $ B.dequeue q
    atomically $ B.enqueue ent q
    enqdeqB q (n - 1)
-}

----------------------------------------------------------------

benchA :: [Int] -> IO ()
benchA xs = do
    q <- atomically A.new
    createA xs q
    enqdeqA q numOfTrials

createA :: [Int] -> A.PriorityQueue Int -> IO ()
createA [] _      = return ()
createA (x:xs) !q = do
    let !ent = A.newEntry x x
    atomically $ A.enqueue ent q
    createA xs q

enqdeqA :: A.PriorityQueue Int -> Int -> IO ()
enqdeqA _ 0  = return ()
enqdeqA q !n = do
    ent <- atomically $ A.dequeue q
    atomically $ A.enqueue ent q
    enqdeqA q (n - 1)

----------------------------------------------------------------
