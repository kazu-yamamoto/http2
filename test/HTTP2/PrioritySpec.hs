{-# LANGUAGE BangPatterns, CPP #-}

module HTTP2.PrioritySpec where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Data.List (group, sort)
import Test.Hspec

import Network.HTTP2.Priority
import qualified Network.HTTP2.Priority.PSQ as P
import Network.HTTP2.Types

spec :: Spec
spec = do
    describe "priority tree" $ do
        it "enqueue and dequeue frames from Firefox properly" $ firefox
    describe "base priority queue" $ do
        it "queues entries based on weight" $ do
            let q = P.enqueue 5 (P.newPrecedence   1) 5 $
                    P.enqueue 3 (P.newPrecedence 101) 3 $
                    P.enqueue 1 (P.newPrecedence 201) 1 P.empty
                xs = enqdeq q 1000
            map length (group (sort xs)) `shouldBe` [664,333,3]
        it "deletes entries properly" $ do
            let q = P.enqueue 5 (P.newPrecedence   1) 5 $
                    P.enqueue 3 (P.newPrecedence 101) 3 $
                    P.enqueue 1 (P.newPrecedence 201) 1 P.empty
                (mv1,q1) = P.delete 1 q
                (mv2,q2) = P.delete 3 q
            P.baseDeficit q `shouldBe` 0
            mv1 `shouldBe` Just (1 :: Int)
            P.baseDeficit q1 `shouldBe` 326
            mv2 `shouldBe` Just 3
            P.baseDeficit q2 `shouldBe` 0

firefox :: IO ()
firefox = do
    pt <- newPriorityTree :: IO (PriorityTree Int)
    prepare pt  3 (pri 0 201)
    prepare pt  5 (pri 0 101)
    prepare pt  7 (pri 0   1)
    prepare pt  9 (pri 7   1)
    prepare pt 11 (pri 3   1)
    enQ pt 13 (pre 11 32)
    deQ pt `shouldReturn` 13
    enQ pt 15 (pre  3 32)
    enQ pt 17 (pre  3 32)
    enQ pt 19 (pre  3 32)
    enQ pt 21 (pre  3 32)
    enQ pt 23 (pre  3 32)
    enQ pt 25 (pre  3 32)
    enQ pt 27 (pre 11 22)
    enQ pt 29 (pre 11 22)
    enQ pt 31 (pre 11 22)
    enQ pt 33 (pre  5 32)
    enQ pt 35 (pre  5 32)
    enQ pt 37 (pre  5 32)
    deQ pt `shouldReturn` 15
    deQ pt `shouldReturn` 33
    deQ pt `shouldReturn` 17
    delete pt 17 (pre  3 32) `shouldReturn` Nothing
    delete pt 31 (pre 11 22) `shouldReturn` Just 31
    deQ pt `shouldReturn` 19
    deQ pt `shouldReturn` 35
    deQ pt `shouldReturn` 21
    deQ pt `shouldReturn` 23
    deQ pt `shouldReturn` 37
    deQ pt `shouldReturn` 25
    deQ pt `shouldReturn` 27
    deQ pt `shouldReturn` 29
    enQ pt 39 (pre  3 32)
    deQ pt `shouldReturn` 39

enQ :: PriorityTree Int -> StreamId -> Precedence -> IO ()
enQ pt sid p = enqueue pt sid p sid

deQ :: PriorityTree Int
    -> IO StreamId
deQ pt = (\(x,_,_) -> x) <$> dequeue pt

pri :: StreamId -> Weight -> Priority
pri dep w = Priority False dep w

pre :: StreamId -> Weight -> Precedence
pre dep w = toPrecedence $ pri dep w

enqdeq :: P.PriorityQueue Int -> Int -> [Int]
enqdeq pq num = loop pq num []
  where
    loop _   0 ks = ks
    loop !q !n ks = case P.dequeue q of
        Nothing         -> error "enqdeq"
        Just (k,p,v,q') -> loop (P.enqueue k p v q') (n - 1) (k:ks)
