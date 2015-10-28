{-# LANGUAGE BangPatterns #-}

module ArrayOfQueueSpec where

import Control.Concurrent.STM
import Data.List (group, sort)
import Test.Hspec

import qualified ArrayOfQueue as P

spec :: Spec
spec = do
    describe "base priority queue" $ do
        it "queues entries based on weight" $ do
            q <- atomically P.new
            atomically $ P.enqueue 1 201 1 q
            atomically $ P.enqueue 3 101 3 q
            atomically $ P.enqueue 5   1 5 q
            xs <- enqdeq q 1000
            map length (group (sort xs)) `shouldBe` [663,334,3]

enqdeq :: P.PriorityQueue Int -> Int -> IO [Int]
enqdeq pq num = loop pq num []
  where
    loop _   0 xs = return xs
    loop !q !n xs = do
        Just (k,w,x) <- atomically $ P.dequeue q
        atomically $ P.enqueue k w x q
        loop q (n - 1) (x:xs)
