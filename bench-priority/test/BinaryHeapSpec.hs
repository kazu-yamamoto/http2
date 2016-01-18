{-# LANGUAGE BangPatterns #-}

module BinaryHeapSpec where

import Data.IORef (readIORef)
import Data.List (group, sort)
import Test.Hspec

import qualified BinaryHeap as P

spec :: Spec
spec = do
    describe "base priority queue" $ do
        it "queues entries based on weight" $ do
            q <- P.new 100
            e1 <- P.newEntry 1 201
            P.enqueue e1 q
            e2 <- P.newEntry 3 101
            P.enqueue e2 q
            e3 <- P.newEntry 5 1
            P.enqueue e3 q
            xs <- enqdeq q 1000
            map length (group (sort xs)) `shouldBe` [664,333,3]
        it "deletes properly" $ do
            q <- P.new 100
            e1 <- P.newEntry 1 201
            e3 <- P.newEntry 3  50
            e5 <- P.newEntry 5   5
            e7 <- P.newEntry 7   1
            P.enqueue e1 q
            P.enqueue e3 q
            P.enqueue e5 q
            P.enqueue e7 q
            i1 <- P.dequeue q
            readIORef (P.item i1) `shouldReturn` 1
            P.delete e5 q
            i3 <- P.dequeue q
            readIORef (P.item i3) `shouldReturn` 3
            i7 <- P.dequeue q
            readIORef (P.item i7) `shouldReturn` 7

enqdeq :: P.PriorityQueue Int -> Int -> IO [Int]
enqdeq pq num = loop pq num []
  where
    loop _   0 vs = return vs
    loop !q !n vs = do
        ent <- P.dequeue q
        P.enqueue ent q
        v <- readIORef $ P.item ent
        loop q (n - 1) (v:vs)
