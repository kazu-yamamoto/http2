module DecodeSpec where

import Network.HPACK.Context
import Network.HPACK.HeaderBlock
import Test.Hspec

import HeaderBlock

spec :: Spec
spec = do
    describe "fromHeaderBlock" $ do
        it "decodes HeaderSet in request" $ do
            (h1,c1) <- newContext 4096 >>= fromHeaderBlock e31
            h1 `shouldBe` e31h
            (h2,c2) <- fromHeaderBlock e32 c1
            h2 `shouldBe` e32h
            (h3,_)  <- fromHeaderBlock e33 c2
            h3 `shouldBe` e33h
        it "decodes HeaderSet in response" $ do
            (h1,c1) <- newContext 256 >>= fromHeaderBlock e51
            h1 `shouldBe` e51h
            (h2,c2) <- fromHeaderBlock e52 c1
            h2 `shouldBe` e52h
            (h3,_)  <- fromHeaderBlock e53 c2
            h3 `shouldBe` e53h
