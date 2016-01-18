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
            P.enqueue 1 201 1 q
            P.enqueue 3 101 3 q
            P.enqueue 5   1 5 q
            xs <- enqdeq q 1000
            map length (group (sort xs)) `shouldBe` [663,334,3]
        it "deletes properly" $ do
            q <- P.new
            P.enqueue 1 201 (1 :: Int) q
            P.enqueue 3  50 3 q
            P.enqueue 5   5 5 q
            P.enqueue 7   1 7 q
            P.dequeue q `shouldReturn`  Just (1,201,1)
            P.delete 5 q `shouldReturn` Just 5
            P.dequeue q `shouldReturn`  Just (3,50,3)
            P.dequeue q `shouldReturn`  Just (7,1,7)

enqdeq :: P.PriorityQueue Int -> Int -> IO [Int]
enqdeq pq num = loop pq num []
  where
    loop _   0 xs = return xs
    loop !q !n xs = do
        Just (k,w,x) <- P.dequeue q
        P.enqueue k w x q
        loop q (n - 1) (x:xs)
