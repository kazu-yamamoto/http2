{-# LANGUAGE OverloadedStrings #-}

module HPACK.HeaderBlockSpec where

import qualified Data.ByteString.Char8 as BS
import Network.HPACK.HeaderBlock
import Test.Hspec
import Test.Hspec.QuickCheck

import HPACK.HeaderBlock

spec :: Spec
spec = do
    describe "toByteStream" $ do
        it "encodes HeaderBlock" $ do
            toByteStream True d41 `shouldBe` d41b
            toByteStream True d42 `shouldBe` d42b
            toByteStream True d43 `shouldBe` d43b
            toByteStream True d61 `shouldBe` d61b
            toByteStream True d62 `shouldBe` d62b
            toByteStream True d63 `shouldBe` d63b

    describe "fromByteStream" $ do
        it "encodes HeaderBlock" $ do
            fromByteStream d41b `shouldBe` Right d41
            fromByteStream d42b `shouldBe` Right d42
            fromByteStream d43b `shouldBe` Right d43
            fromByteStream d61b `shouldBe` Right d61
            fromByteStream d62b `shouldBe` Right d62
            fromByteStream d63b `shouldBe` Right d63

    describe "toByteStream & fromByteStream" $ do
        prop "duality for request" $ \k v -> do
            let key = BS.pack ('k':k)
                val = BS.pack ('v':v)
                hb = [Literal Add (Lit key) val]
            fromByteStream (toByteStream True hb) `shouldBe` Right hb
        prop "duality for response" $ \v -> do
            let val = BS.pack ('v':v)
                hb = [Literal Add (Idx 3) val]
            fromByteStream (toByteStream True hb) `shouldBe` Right hb
