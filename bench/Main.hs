{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent.STM
import Criterion.Main
import Data.List (foldl')
import System.Random

import qualified ArrayOfQueue as A
import qualified ArrayOfQueueIO as AIO
import qualified BinaryHeap as B
import qualified BinaryHeapIO as BIO
import qualified Heap as O
import qualified Network.HTTP2.Priority.PSQ as P
import qualified RandomSkewHeap as R

numOfStreams :: Int
numOfStreams = 100

numOfTrials :: Int
numOfTrials = 10000

main :: IO ()
main = do
    gen <- getStdGen
    let ws = take numOfStreams $ randomRs (1,256) gen
        ks = [1,3..]
        xs = zip ks ws
    defaultMain [
        bgroup "enqueue & dequeue" [
              bench "Random Skew Heap"      $ whnf enqdeqR ws
            , bench "Okasaki Heap"          $ whnf enqdeqO xs
            , bench "Priority Search Queue" $ whnf enqdeqP xs
            , bench "Binary Heap STM"       $ nfIO (enqdeqB ws)
            , bench "Binary Heap IO"        $ nfIO (enqdeqBIO ws)
            , bench "Array of Queue STM"    $ nfIO (enqdeqA ws)
            , bench "Array of Queue IO"     $ nfIO (enqdeqAIO ws)
            ]
      , bgroup "delete" [
              bench "Okasaki Heap"          $ whnf deleteO xs
            , bench "Priority Search Queue" $ whnf deleteP xs
            ]
      ]

----------------------------------------------------------------

enqdeqR :: [Int] -> ()
enqdeqR ys = loop pq numOfTrials
  where
    !pq = createR ys R.empty
    loop _ 0  = ()
    loop q !n = case R.dequeue q of
        Nothing -> error "enqdeqR"
        Just (ent,q') -> let !q'' = R.enqueue ent q'
                         in loop q'' (n - 1)

createR :: [Int] -> R.PriorityQueue Int -> R.PriorityQueue Int
createR [] !q = q
createR (x:xs) !q = createR xs q'
  where
    !ent = R.newEntry x x
    !q' = R.enqueue ent q

----------------------------------------------------------------

enqdeqO :: [(Int,Int)] -> O.PriorityQueue Int
enqdeqO xs = loop pq numOfTrials
  where
    !pq = createO xs O.empty
    loop !q  0 = q
    loop !q !n = case O.dequeue q of
        Nothing -> error "enqdeqO"
        Just (k,ent,q') -> loop (O.enqueue k ent q') (n - 1)

deleteO :: [(Int,Int)] -> O.PriorityQueue Int
deleteO xs = foldl' (flip O.delete) pq ks
  where
    !pq = createO xs O.empty
    (ks,_) = unzip xs

createO :: [(Int,Int)] -> O.PriorityQueue Int -> O.PriorityQueue Int
createO [] !q = q
createO ((k,w):xs) !q = createO xs q'
  where
    !ent = O.newEntry k w
    !q' = O.enqueue k ent q

----------------------------------------------------------------

enqdeqP :: [(Int,Int)] -> P.PriorityQueue Int
enqdeqP xs = loop pq numOfTrials
  where
    !pq = createP xs P.empty
    loop !q 0  = q
    loop !q !n = case P.dequeue q of
        Nothing -> error "enqdeqP"
        Just (k,ent,q') -> loop (P.enqueue k ent q') (n - 1)

deleteP :: [(Int,Int)] -> P.PriorityQueue Int
deleteP xs = foldl' (flip P.delete) pq ks
  where
    !pq = createP xs P.empty
    (ks,_) = unzip xs

createP :: [(Int,Int)] -> P.PriorityQueue Int -> P.PriorityQueue Int
createP [] !q = q
createP ((k,w):xs) !q = createP xs q'
  where
    !ent = P.newEntry k w
    !q' = P.enqueue k ent q

----------------------------------------------------------------

enqdeqB :: [Int] -> IO ()
enqdeqB xs = do
    q <- atomically (B.new numOfStreams)
    createB xs q
    loop q numOfTrials
  where
    loop _ 0  = return ()
    loop q !n = do
        ent <- atomically $ B.dequeue q
        atomically $ B.enqueue ent q
        loop q (n - 1)

createB :: [Int] -> B.PriorityQueue Int -> IO ()
createB [] _      = return ()
createB (x:xs) !q = do
    let !ent = B.newEntry x x
    atomically $ B.enqueue ent q
    createB xs q

----------------------------------------------------------------

enqdeqBIO :: [Int] -> IO ()
enqdeqBIO xs = do
    q <- BIO.new numOfStreams
    createBIO xs q
    loop q numOfTrials
  where
    loop _ 0  = return ()
    loop q !n = do
        ent <- BIO.dequeue q
        BIO.enqueue ent q
        loop q (n - 1)

createBIO :: [Int] -> BIO.PriorityQueue Int -> IO ()
createBIO [] _      = return ()
createBIO (x:xs) !q = do
    let !ent = BIO.newEntry x x
    BIO.enqueue ent q
    createBIO xs q

----------------------------------------------------------------

enqdeqA :: [Int] -> IO ()
enqdeqA xs = do
    q <- atomically A.new
    createA xs q
    loop q numOfTrials
  where
    loop _ 0  = return ()
    loop q !n = do
        ent <- atomically $ A.dequeue q
        atomically $ A.enqueue ent q
        loop q (n - 1)

createA :: [Int] -> A.PriorityQueue Int -> IO ()
createA [] _      = return ()
createA (x:xs) !q = do
    let !ent = A.newEntry x x
    atomically $ A.enqueue ent q
    createA xs q

----------------------------------------------------------------

enqdeqAIO :: [Int] -> IO ()
enqdeqAIO xs = do
    q <- AIO.new
    createAIO xs q
    loop q numOfTrials
  where
    loop _ 0  = return ()
    loop q !n = do
        ent <- AIO.dequeue q
        AIO.enqueue ent q
        loop q (n - 1)

createAIO :: [Int] -> AIO.PriorityQueue Int -> IO ()
createAIO [] _      = return ()
createAIO (x:xs) !q = do
    let !ent = AIO.newEntry x x
    AIO.enqueue ent q
    createAIO xs q

----------------------------------------------------------------
