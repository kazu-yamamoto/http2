{-# LANGUAGE BangPatterns #-}

module HeapSpec where

import Data.List (group, sort)
import Test.Hspec

import qualified Heap as P

spec :: Spec
spec = do
    describe "base priority queue" $ do
        it "queues entries based on weight" $ do
            let q = P.enqueue 5   1 5 $
                    P.enqueue 3 101 3 $
                    P.enqueue 1 201 1 P.empty
                xs = enqdeq q 1000
            map length (group (sort xs)) `shouldBe` [664,333,3]
        it "deletes properly" $ do
            let q = P.enqueue 7   1 7 $
                    P.enqueue 5   5 5 $
                    P.enqueue 3  50 3 $
                    P.enqueue 1 201 (1 :: Int) P.empty
            let Just (k1,_,_,q1) = P.dequeue q
            k1 `shouldBe` 1
            let (mk, q2) = P.delete 5 q1
            mk `shouldBe` Just 5
            let Just (k3,_,_,q3) = P.dequeue q2
            k3 `shouldBe` 3
            let Just (k4,_,_,_) = P.dequeue q3
            k4 `shouldBe` 7

enqdeq :: P.PriorityQueue Int -> Int -> [Int]
enqdeq pq num = loop pq num []
  where
    loop _   0 xs = xs
    loop !q !n xs = case P.dequeue q of
        Nothing         -> error "enqdeq"
        Just (k,w,x,q') -> loop (P.enqueue k w x q') (n - 1) (x:xs)
