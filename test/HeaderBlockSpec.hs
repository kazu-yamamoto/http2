{-# LANGUAGE OverloadedStrings #-}

module HeaderBlockSpec where

import qualified Data.ByteString.Char8 as BS
import Network.HPACK.HeaderBlock
import Test.Hspec
import Test.Hspec.QuickCheck

import HeaderBlock

spec :: Spec
spec = do
    describe "toByteStream" $ do
        it "encodes HeaderBlock" $ do
            toByteStream True e31 `shouldBe` e31b
            toByteStream True e32 `shouldBe` e32b
            toByteStream True e33 `shouldBe` e33b
            toByteStream True e51 `shouldBe` e51b
            toByteStream True e52 `shouldBe` e52b
            toByteStream True e53 `shouldBe` e53b

    describe "fromByteStream" $ do
        it "encodes HeaderBlock" $ do
            fromByteStream e31b `shouldBe` Right e31
            fromByteStream e32b `shouldBe` Right e32
            fromByteStream e33b `shouldBe` Right e33
            fromByteStream e51b `shouldBe` Right e51
            fromByteStream e52b `shouldBe` Right e52
            fromByteStream e53b `shouldBe` Right e53
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
