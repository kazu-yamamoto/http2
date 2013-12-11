module DecodeSpec where

import Network.HPACK.Context
import Network.HPACK.HeaderBlock
import Test.Hspec

import HeaderBlock

spec :: Spec
spec = do
    describe "fromHeaderBlock" $ do
        it "decodes HeaderSet in request" $ do
            (h1,c1) <- newContext 4096 >>= fromHeaderBlock e211
            h1 `shouldBe` e211h
            (h2,c2) <- fromHeaderBlock e212 c1
            h2 `shouldBe` e212h
            (h3,_)  <- fromHeaderBlock e213 c2
            h3 `shouldBe` e213h
        it "decodes HeaderSet in response" $ do
            (h1,c1) <- newContext 256 >>= fromHeaderBlock e411
            h1 `shouldBe` e411h
            (h2,c2) <- fromHeaderBlock e412 c1
            h2 `shouldBe` e412h
            (h3,_)  <- fromHeaderBlock e413 c2
            h3 `shouldBe` e413h
