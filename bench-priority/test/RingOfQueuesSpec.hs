{-# LANGUAGE BangPatterns #-}

module RingOfQueuesSpec where

import Data.List (group, sort)
import Test.Hspec

import qualified RingOfQueues as P

spec :: Spec
spec = do
    describe "base priority queue" $ do
        it "queues entries based on weight" $ do
            q <- P.new
            P.enqueue (P.newEntry 1 201) q
            P.enqueue (P.newEntry 3 101) q
            P.enqueue (P.newEntry 5   1) q
            xs <- enqdeq q 1000
            map length (group (sort xs)) `shouldBe` [663,334,3]
        it "deletes properly" $ do
            q <- P.new :: IO (P.PriorityQueue Int)
            P.enqueue (P.newEntry 1 201) q
            P.enqueue (P.newEntry 3  50) q
            node5 <- P.enqueue (P.newEntry 5 5) q
            P.enqueue (P.newEntry 7   1) q
            (P.item <$>) <$> P.dequeue q `shouldReturn`  Just 1
            P.delete node5
            (P.item <$>) <$> P.dequeue q `shouldReturn`  Just 3
            (P.item <$>) <$> P.dequeue q `shouldReturn`  Just 7

enqdeq :: P.PriorityQueue Int -> Int -> IO [Int]
enqdeq pq num = loop pq num []
  where
    loop _   0 vs = return vs
    loop !q !n vs = do
        Just ent <- P.dequeue q
        P.enqueue ent q
        let !v = P.item ent
        loop q (n - 1) (v:vs)
