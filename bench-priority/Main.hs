{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent.STM
import Gauge.Main
import Data.List (foldl')
import System.Random.MWC

import qualified RingOfQueuesSTM as A
import qualified RingOfQueues as AIO
import qualified BinaryHeapSTM as B
import qualified BinaryHeap as BIO
import qualified Heap as O
import qualified Network.HTTP2.Priority.PSQ as P
import qualified RandomSkewHeap as R

type Key = Int
type Weight = Int

numOfStreams :: Int
numOfStreams = 100

numOfTrials :: Int
numOfTrials = 10000

main :: IO ()
main = do
    gen <- create
    ws <- uniformRs (1,256) gen numOfStreams
    let ks = [1,3..]
        xs = zip ks ws
    defaultMain [
        bgroup "enqueue & dequeue" [
              bench "Random Skew Heap"      $ whnf enqdeqR xs
            , bench "Skew Binomial Heap"    $ whnf enqdeqO xs
            , bench "Priority Search Queue" $ whnf enqdeqP xs
            , bench "Binary Heap"           $ nfIO (enqdeqBIO xs)
            , bench "Binary Heap STM"       $ nfIO (enqdeqB xs)
            , bench "Ring of Queues"        $ nfIO (enqdeqAIO xs)
            , bench "Ring of Queues STM"    $ nfIO (enqdeqA xs)
            ]
      , bgroup "delete" [
              bench "Random Skew Heap"      $ whnf deleteR xs
            , bench "Skew Binomial Heap"    $ whnf deleteO xs
            , bench "Priority Search Queue" $ whnf deleteP xs
            , bench "Binary Heap"           $ nfIO (deleteBIO xs)
            , bench "Binary Heap STM"       $ nfIO (deleteB xs)
            , bench "Ring of Queues IO"     $ nfIO (deleteAIO xs)
            ]
      ]
  where
    uniformRs range gen n = loop n []
      where
        loop 0 rs = return rs
        loop i rs = do
            r <- uniformR range gen
            loop (i-1) (r:rs)

----------------------------------------------------------------

enqdeqR :: [(Key,Weight)] -> ()
enqdeqR xs = loop pq numOfTrials
  where
    !pq = createR xs R.empty
    loop _ 0  = ()
    loop q !n = case R.dequeue q of
        Nothing -> error "enqdeqR"
        Just (k,w,v,q') -> let !q'' = R.enqueue k w v q'
                           in loop q'' (n - 1)

deleteR :: [(Key,Weight)] -> R.PriorityQueue Int
deleteR xs = foldl' (\q k -> let (_,!q') = R.delete k q in q') pq ks
  where
    !pq = createR xs R.empty
    (ks,_) = unzip xs

createR :: [(Key,Weight)] -> R.PriorityQueue Int -> R.PriorityQueue Int
createR [] !q = q
createR ((k,w):xs) !q = createR xs q'
  where
    !v = k
    !q' = R.enqueue k w v q

----------------------------------------------------------------

enqdeqO :: [(Key,Weight)] -> O.PriorityQueue Int
enqdeqO xs = loop pq numOfTrials
  where
    !pq = createO xs O.empty
    loop !q  0 = q
    loop !q !n = case O.dequeue q of
        Nothing -> error "enqdeqO"
        Just (k,p,v,q') -> loop (O.enqueue k p v q') (n - 1)

deleteO :: [(Key,Weight)] -> O.PriorityQueue Int
deleteO xs = foldl' (\q k -> let (_,!q') = O.delete k q in q') pq ks
  where
    !pq = createO xs O.empty
    (ks,_) = unzip xs

createO :: [(Key,Weight)] -> O.PriorityQueue Int -> O.PriorityQueue Int
createO [] !q = q
createO ((k,w):xs) !q = createO xs q'
  where
    !pre = O.newPrecedence w
    !v = k
    !q' = O.enqueue k pre v q

----------------------------------------------------------------

enqdeqP :: [(Key,Weight)] -> P.PriorityQueue Int
enqdeqP xs = loop pq numOfTrials
  where
    !pq = createP xs P.empty
    loop !q 0  = q
    loop !q !n = case P.dequeue q of
        Nothing -> error "enqdeqP"
        Just (k,pre,x,q') -> loop (P.enqueue k pre x q') (n - 1)

deleteP :: [(Key,Weight)] -> P.PriorityQueue Int
deleteP xs = foldl' (\q k -> let (_,!q') = P.delete k q in q') pq ks
  where
    !pq = createP xs P.empty
    (ks,_) = unzip xs

createP :: [(Key,Weight)] -> P.PriorityQueue Int -> P.PriorityQueue Int
createP [] !q = q
createP ((k,w):xs) !q = createP xs q'
  where
    !pre = P.newPrecedence w
    !v = k
    !q' = P.enqueue k pre v q

----------------------------------------------------------------

enqdeqB :: [(Key,Weight)] -> IO ()
enqdeqB xs = do
    q <- atomically (B.new numOfStreams)
    _ <- createB xs q
    loop q numOfTrials
  where
    loop _ 0  = return ()
    loop q !n = do
        ent <- atomically $ B.dequeue q
        atomically $ B.enqueue ent q
        loop q (n - 1)

deleteB :: [(Key,Weight)] -> IO ()
deleteB xs = do
    q <- atomically $ B.new numOfStreams
    ents <- createB xs q
    mapM_ (\ent -> atomically $ B.delete ent q) ents

createB :: [(Key,Weight)] -> B.PriorityQueue Int -> IO ([B.Entry Key])
createB []          _ = return []
createB ((k,w):xs) !q = do
    ent <- atomically $ B.newEntry k w
    atomically $ B.enqueue ent q
    ents <- createB xs q
    return $ ent:ents

----------------------------------------------------------------

enqdeqBIO :: [(Key,Weight)] -> IO ()
enqdeqBIO xs = do
    q <- BIO.new numOfStreams
    _ <- createBIO xs q
    loop q numOfTrials
  where
    loop _ 0  = return ()
    loop q !n = do
        ent <- BIO.dequeue q
        BIO.enqueue ent q
        loop q (n - 1)

deleteBIO :: [(Key,Weight)] -> IO ()
deleteBIO xs = do
    q <- BIO.new numOfStreams
    ents <- createBIO xs q
    mapM_ (\ent -> BIO.delete ent q) ents

createBIO :: [(Key,Weight)] -> BIO.PriorityQueue Int -> IO ([BIO.Entry Key])
createBIO []          _ = return []
createBIO ((k,w):xs) !q = do
    ent <- BIO.newEntry k w
    BIO.enqueue ent q
    ents <- createBIO xs q
    return $ ent:ents

----------------------------------------------------------------

enqdeqA :: [(Key,Weight)] -> IO ()
enqdeqA ws = do
    q <- atomically A.new
    createA ws q
    loop q numOfTrials
  where
    loop _ 0  = return ()
    loop q !n = do
        ent <- atomically $ A.dequeue q
        atomically $ A.enqueue ent q
        loop q (n - 1)

createA :: [(Key,Weight)] -> A.PriorityQueue Int -> IO ()
createA [] _          = return ()
createA ((k,w):xs) !q = do
    let !ent = A.newEntry k w
    atomically $ A.enqueue ent q
    createA xs q

----------------------------------------------------------------

enqdeqAIO :: [(Key,Weight)] -> IO ()
enqdeqAIO xs = do
    q <- AIO.new
    _ <- createAIO xs q
    loop q numOfTrials
  where
    loop _ 0  = return ()
    loop q !n = do
        Just ent <- AIO.dequeue q
        _ <- AIO.enqueue ent q
        loop q (n - 1)

deleteAIO :: [(Key,Weight)] -> IO ()
deleteAIO xs = do
    q <- AIO.new
    ns <- createAIO xs q
    mapM_ AIO.delete ns

createAIO :: [(Key,Weight)] -> AIO.PriorityQueue Int -> IO [AIO.Node (AIO.Entry Weight)]
createAIO []          _ = return []
createAIO ((k,w):xs) !q = do
    let !ent = AIO.newEntry k w
    n <- AIO.enqueue ent q
    ns <- createAIO xs q
    return $ n : ns

----------------------------------------------------------------
