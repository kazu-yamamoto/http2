{-# LANGUAGE BangPatterns #-}

module BinaryHeapSpec where

import Control.Concurrent.STM
import Data.IORef (readIORef)
import Data.List (group, sort)
import Test.Hspec

import qualified BinaryHeap as P

spec :: Spec
spec = do
    describe "base priority queue" $ do
        it "queues entries based on weight" $ do
            q <- atomically $ P.new 100
            e1 <- atomically $ P.newEntry 1 201
            atomically $ P.enqueue e1 q
            e2 <- atomically $ P.newEntry 3 101
            atomically $ P.enqueue e2 q
            e3 <- atomically $ P.newEntry 5 1
            atomically $ P.enqueue e3 q
            xs <- enqdeq q 1000
            map length (group (sort xs)) `shouldBe` [664,333,3]
        it "deletes properly" $ do
            q <- atomically $ P.new 100
            e1 <- atomically $ P.newEntry 1 201
            e3 <- atomically $ P.newEntry 3  50
            e5 <- atomically $ P.newEntry 5   5
            e7 <- atomically $ P.newEntry 7   1
            atomically $ P.enqueue e1 q
            atomically $ P.enqueue e3 q
            atomically $ P.enqueue e5 q
            atomically $ P.enqueue e7 q
            i1 <- atomically $ P.dequeue q
            atomically (readTVar (P.item i1)) `shouldReturn` 1
            atomically $ P.delete e5 q
            i3 <- atomically $ P.dequeue q
            atomically (readTVar (P.item i3)) `shouldReturn` 3
            i7 <- atomically $ P.dequeue q
            atomically (readTVar (P.item i7)) `shouldReturn` 7

enqdeq :: P.PriorityQueue Int -> Int -> IO [Int]
enqdeq pq num = loop pq num []
  where
    loop _   0 vs = return vs
    loop !q !n vs = do
        ent <- atomically $ P.dequeue q
        atomically $ P.enqueue ent q
        v <- atomically . readTVar $ P.item ent
        loop q (n - 1) (v:vs)
