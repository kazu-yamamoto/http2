{-# LANGUAGE OverloadedStrings #-}

module HPACK.HeaderBlockSpec where

import qualified Data.ByteString.Char8 as BS
import Network.HPACK.HeaderBlock
import Test.Hspec
import Test.Hspec.QuickCheck

import HPACK.HeaderBlock

spec :: Spec
spec = do
    describe "toByteString" $ do
        it "encodes HeaderBlock" $ do
            toByteString True d41 `shouldBe` d41b
            toByteString True d42 `shouldBe` d42b
            toByteString True d43 `shouldBe` d43b
            toByteString True d61 `shouldBe` d61b
            toByteString True d62 `shouldBe` d62b
            toByteString True d63 `shouldBe` d63b

    describe "fromByteString" $ do
        it "encodes HeaderBlock" $ do
            fromByteString d41b `shouldBe` Right d41
            fromByteString d42b `shouldBe` Right d42
            fromByteString d43b `shouldBe` Right d43
            fromByteString d61b `shouldBe` Right d61
            fromByteString d62b `shouldBe` Right d62
            fromByteString d63b `shouldBe` Right d63

    describe "toByteString & fromByteString" $ do
        prop "duality for request" $ \k v -> do
            let key = BS.pack ('k':k)
                val = BS.pack ('v':v)
                hb = [Literal Add (Lit key) val]
            fromByteString (toByteString True hb) `shouldBe` Right hb
        prop "duality for response" $ \v -> do
            let val = BS.pack ('v':v)
                hb = [Literal Add (Idx 3) val]
            fromByteString (toByteString True hb) `shouldBe` Right hb
