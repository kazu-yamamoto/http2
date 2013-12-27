module DecodeSpec where

import Network.HPACK.Context
import Network.HPACK.HeaderBlock
import Test.Hspec

import HeaderBlock

spec :: Spec
spec = do
    describe "fromHeaderBlock" $ do
        it "decodes HeaderSet in request" $ do
            (c1,h1) <- newContext 4096 >>= flip fromHeaderBlock e31
            h1 `shouldBe` e31h
            (c2,h2) <- fromHeaderBlock c1 e32
            h2 `shouldBe` e32h
            (_,h3)  <- fromHeaderBlock c2 e33
            h3 `shouldBe` e33h
        it "decodes HeaderSet in response" $ do
            (c1,h1) <- newContext 256 >>= flip fromHeaderBlock e51
            h1 `shouldBe` e51h
            (c2,h2) <- fromHeaderBlock c1 e52
            h2 `shouldBe` e52h
            (_,h3)  <- fromHeaderBlock c2 e53
            h3 `shouldBe` e53h
