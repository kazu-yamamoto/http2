{-# LANGUAGE BangPatterns #-}

module RandomSkewHeapSpec where

import Data.List (group, sort)
import Test.Hspec

import qualified RandomSkewHeap as P

spec :: Spec
spec = do
    describe "base priority queue" $ do
        it "queues entries based on weight" $ do
            let q = P.enqueue 5   1 5 $
                    P.enqueue 3 101 3 $
                    P.enqueue 1 201 1 P.empty
                xs = enqdeq q 1000
            let [x201,x101,x1] = map length (group (sort xs))
            x201 `shouldSatisfy` (\x -> 640 <= x && x <= 690)
            x101 `shouldSatisfy` (\x -> 310 <= x && x <= 360)
            x1   `shouldSatisfy` (\x ->   1 <= x && x <=  10)

enqdeq :: P.PriorityQueue Int -> Int -> [Int]
enqdeq pq num = loop pq num []
  where
    loop _   0 xs = xs
    loop !q !n xs = case P.dequeue q of
        Nothing         -> error "enqdeq"
        Just (k,w,x,q') -> loop (P.enqueue k w x q') (n - 1) (x:xs)
