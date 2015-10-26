module HTTP2.PrioritySpec where

import Test.Hspec

import Network.HTTP2.Priority
import qualified Network.HTTP2.Priority.PSQ as P
import Network.HTTP2.Types

spec :: Spec
spec = do
    describe "enqueue & dequeue" $ do
        it "enqueue and dequeue frames from Firefox properly" $ do
            pt <- newPriorityTree :: IO (PriorityTree Int)
            prepare pt  3 (pri 0 201)
            prepare pt  5 (pri 0 101)
            prepare pt  7 (pri 0   1)
            prepare pt  9 (pri 7   1)
            prepare pt 11 (pri 3   1)
            enQ pt 13 (pri 11 32)
            dequeue pt `shouldReturn` (13,13)
            enQ pt 15 (pri  3 32)
            enQ pt 17 (pri  3 32)
            enQ pt 19 (pri  3 32)
            enQ pt 21 (pri  3 32)
            enQ pt 23 (pri  3 32)
            enQ pt 25 (pri  3 32)
            enQ pt 27 (pri 11 22)
            enQ pt 29 (pri 11 22)
            enQ pt 31 (pri 11 22)
            enQ pt 33 (pri  5 32)
            enQ pt 35 (pri  5 32)
            enQ pt 37 (pri  5 32)
            dequeue pt `shouldReturn` (15,15)
            dequeue pt `shouldReturn` (33,33)
            dequeue pt `shouldReturn` (17,17)
            dequeue pt `shouldReturn` (19,19)
            dequeue pt `shouldReturn` (35,35)
            dequeue pt `shouldReturn` (21,21)
            dequeue pt `shouldReturn` (23,23)
            dequeue pt `shouldReturn` (37,37)
            dequeue pt `shouldReturn` (25,25)
            dequeue pt `shouldReturn` (27,27)
            dequeue pt `shouldReturn` (29,29)
            dequeue pt `shouldReturn` (31,31)

enQ :: PriorityTree Int -> StreamId -> Priority -> IO ()
enQ pt sid p = enqueue pt sid p sid

pri :: StreamId -> Weight -> Priority
pri dep w = Priority False dep w
