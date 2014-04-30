module DecodeSpec where

import Data.List (sort)
import Network.HPACK.Context
import Network.HPACK.HeaderBlock
import Test.Hspec

import HeaderBlock

shouldBeSameAs :: (Ord a, Show a) => [a] -> [a] -> Expectation
shouldBeSameAs xs ys = sort xs `shouldBe` sort ys

spec :: Spec
spec = do
    describe "fromHeaderBlock" $ do
        it "decodes HeaderSet in request" $ do
            (c1,h1) <- newContextForDecoding 4096 >>= flip fromHeaderBlock d41
            h1 `shouldBeSameAs` d41h
            (c2,h2) <- fromHeaderBlock c1 d42
            h2 `shouldBeSameAs` d42h
            (_,h3)  <- fromHeaderBlock c2 d43
            h3 `shouldBeSameAs` d43h
        it "decodes HeaderSet in response" $ do
            (c1,h1) <- newContextForDecoding 256 >>= flip fromHeaderBlock d61
            h1 `shouldBeSameAs` d61h
            (c2,h2) <- fromHeaderBlock c1 d62
            h2 `shouldBeSameAs` d62h
            (_,h3)  <- fromHeaderBlock c2 d63
            h3 `shouldBeSameAs` d63h
