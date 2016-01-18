{-# LANGUAGE BangPatterns #-}

module RingOfQueuesSTMSpec where

import Control.Concurrent.STM
import Data.IORef (readIORef)
import Data.List (group, sort)
import Test.Hspec

import qualified RingOfQueuesSTM as P

spec :: Spec
spec = do
    describe "base priority queue" $ do
        it "queues entries based on weight" $ do
            q <- atomically P.new
            let e1 = P.newEntry 1 201
            atomically $ P.enqueue e1 q
            let e2 = P.newEntry 3 101
            atomically $ P.enqueue e2 q
            let e3 = P.newEntry 5 1
            atomically $ P.enqueue e3 q
            xs <- enqdeq q 1000
            map length (group (sort xs)) `shouldBe` [664,333,3]

enqdeq :: P.PriorityQueue Int -> Int -> IO [Int]
enqdeq pq num = loop pq num []
  where
    loop _   0 vs = return vs
    loop !q !n vs = do
        ent <- atomically $ P.dequeue q
        atomically $ P.enqueue ent q
        let !v = P.item ent
        loop q (n - 1) (v:vs)
