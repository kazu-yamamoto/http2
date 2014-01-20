{-# LANGUAGE OverloadedStrings #-}

module HeaderBlockSpec where

import qualified Data.ByteString.Char8 as BS
import Network.HPACK.HeaderBlock
import Network.HPACK.Huffman
import Test.Hspec
import Test.Hspec.QuickCheck

import HeaderBlock

spec :: Spec
spec = do
    describe "toByteStream" $ do
        it "encodes HeaderBlock" $ do
            toByteStream huffmanEncodeInRequest  True e31 `shouldBe` e31b
            toByteStream huffmanEncodeInRequest  True e32 `shouldBe` e32b
            toByteStream huffmanEncodeInRequest  True e33 `shouldBe` e33b
            toByteStream huffmanEncodeInResponse True e51 `shouldBe` e51b
            toByteStream huffmanEncodeInResponse True e52 `shouldBe` e52b
            toByteStream huffmanEncodeInResponse True e53 `shouldBe` e53b

    describe "fromByteStream" $ do
        it "encodes HeaderBlock" $ do
            fromByteStream huffmanDecodeInRequest  e31b `shouldBe` Right e31
            fromByteStream huffmanDecodeInRequest  e32b `shouldBe` Right e32
            fromByteStream huffmanDecodeInRequest  e33b `shouldBe` Right e33
            fromByteStream huffmanDecodeInResponse e51b `shouldBe` Right e51
            fromByteStream huffmanDecodeInResponse e52b `shouldBe` Right e52
            fromByteStream huffmanDecodeInResponse e53b `shouldBe` Right e53
    describe "toByteStream & fromByteStream" $ do
        prop "duality for request" $ \k v -> do
            let key = BS.pack ('k':k)
                val = BS.pack ('v':v)
                hb = [Literal Add (Lit key) val]
            fromByteStream huffmanDecodeInRequest
                (toByteStream huffmanEncodeInRequest True hb) `shouldBe` Right hb
        prop "duality for response" $ \v -> do
            let val = BS.pack ('v':v)
                hb = [Literal Add (Idx 3) val]
            fromByteStream huffmanDecodeInResponse
                (toByteStream huffmanEncodeInResponse True hb) `shouldBe` Right hb
