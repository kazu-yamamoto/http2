{-# LANGUAGE BangPatterns #-}

module HeapSpec where

import Data.List (group, sort)
import Test.Hspec

import qualified Heap as P

spec :: Spec
spec = do
    describe "base priority queue" $ do
        it "queues entries based on weight" $ do
            let q =
                    P.enqueue 5 (P.newPrecedence 1) 5 $
                        P.enqueue 3 (P.newPrecedence 101) 3 $
                            P.enqueue 1 (P.newPrecedence 201) 1 P.empty
                xs = enqdeq q 1000
            map length (group (sort xs)) `shouldBe` [664, 333, 3]
        it "deletes properly" $ do
            let q =
                    P.enqueue 7 (P.newPrecedence 1) 7 $
                        P.enqueue 5 (P.newPrecedence 5) 5 $
                            P.enqueue 3 (P.newPrecedence 50) 3 $
                                P.enqueue 1 (P.newPrecedence 201) (1 :: Int) P.empty
            let Just (k1, _, _, q1) = P.dequeue q
            k1 `shouldBe` 1
            let (mk, q2) = P.delete 5 q1
            mk `shouldBe` Just 5
            let Just (k3, _, _, q3) = P.dequeue q2
            k3 `shouldBe` 3
            let Just (k4, _, _, _) = P.dequeue q3
            k4 `shouldBe` 7
        it "deletes entries properly" $ do
            let q =
                    P.enqueue 5 (P.newPrecedence 1) 5 $
                        P.enqueue 3 (P.newPrecedence 101) 3 $
                            P.enqueue 1 (P.newPrecedence 201) 1 P.empty
                (mv1, q1) = P.delete 1 q
                (mv2, q2) = P.delete 3 q
            P.baseDeficit q `shouldBe` 0
            mv1 `shouldBe` Just (1 :: Int)
            P.baseDeficit q1 `shouldBe` 326
            mv2 `shouldBe` Just 3
            P.baseDeficit q2 `shouldBe` 0

enqdeq :: P.PriorityQueue Int -> Int -> [Int]
enqdeq pq num = loop pq num []
  where
    loop _ 0 xs = xs
    loop !q !n xs = case P.dequeue q of
        Nothing -> error "enqdeq"
        Just (k, p, x, q') -> loop (P.enqueue k p x q') (n - 1) (x : xs)
