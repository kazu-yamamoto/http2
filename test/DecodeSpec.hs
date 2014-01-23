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
            (c1,h1) <- newContextForDecoding 4096 >>= flip fromHeaderBlock e31
            h1 `shouldBeSameAs` e31h
            (c2,h2) <- fromHeaderBlock c1 e32
            h2 `shouldBeSameAs` e32h
            (_,h3)  <- fromHeaderBlock c2 e33
            h3 `shouldBeSameAs` e33h
        it "decodes HeaderSet in response" $ do
            (c1,h1) <- newContextForDecoding 256 >>= flip fromHeaderBlock e51
            h1 `shouldBeSameAs` e51h
            (c2,h2) <- fromHeaderBlock c1 e52
            h2 `shouldBeSameAs` e52h
            (_,h3)  <- fromHeaderBlock c2 e53
            h3 `shouldBeSameAs` e53h
