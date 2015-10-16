module HTTP2.PrioritySpec where

import Test.Hspec

import Control.Monad (void)
import Network.HTTP2.Priority
import Network.HTTP2.Types

spec :: Spec
spec = do
    describe "enqueue & dequeue" $ do
        it "enqueue and dequeue frames from Firefox properly" $ do
            pt <- newPriorityTree :: IO (PriorityTree Int)
            prepare pt 3 $ Priority False 0 201
            prepare pt 5 $ Priority False 0 101
            prepare pt 7 $ Priority False 0 1
            prepare pt 9 $ Priority False 7 1
            prepare pt 11 $ Priority False 3 1
            enQ pt 13 $ Priority False 11 32
            sid13 <- dequeue pt
            item sid13 `shouldBe` 13
            enQ pt 15 $ Priority False 3 32
            enQ pt 17 $ Priority False 3 32
            enQ pt 19 $ Priority False 3 32
            enQ pt 21 $ Priority False 3 32
            enQ pt 23 $ Priority False 3 32
            enQ pt 25 $ Priority False 3 32
            enQ pt 27 $ Priority False 11 22
            enQ pt 29 $ Priority False 11 22
            enQ pt 31 $ Priority False 11 22
            enQ pt 33 $ Priority False 5 32
            enQ pt 35 $ Priority False 5 32
            enQ pt 37 $ Priority False 5 32
            -- Currently, just checking no errors.
            void $ dequeue pt
            void $ dequeue pt
            void $ dequeue pt
            void $ dequeue pt
            void $ dequeue pt
            void $ dequeue pt
            void $ dequeue pt
            void $ dequeue pt
            void $ dequeue pt
            void $ dequeue pt
            void $ dequeue pt
            void $ dequeue pt

enQ :: PriorityTree Int -> Int -> Priority -> IO ()
enQ pt x p = enqueue pt (newEntry x p) p
