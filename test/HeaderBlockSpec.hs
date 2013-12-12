{-# LANGUAGE OverloadedStrings #-}

module HeaderBlockSpec where

import Network.HPACK.HeaderBlock
import Network.HPACK.Huffman
import Test.Hspec

import HeaderBlock

spec :: Spec
spec = do
    describe "toByteStream" $ do
        it "encodes HeaderBlock" $ do
            toByteStream huffmanEncodeInRequest  e31 `shouldBe` e31b
            toByteStream huffmanEncodeInRequest  e32 `shouldBe` e32b
            toByteStream huffmanEncodeInRequest  e33 `shouldBe` e33b
            toByteStream huffmanEncodeInResponse e51 `shouldBe` e51b
            toByteStream huffmanEncodeInResponse e52 `shouldBe` e52b
            toByteStream huffmanEncodeInResponse e53 `shouldBe` e53b

    describe "fromByteStream" $ do
        it "encodes HeaderBlock" $ do
            fromByteStream huffmanDecodeInRequest  e31b `shouldBe` e31
            fromByteStream huffmanDecodeInRequest  e32b `shouldBe` e32
            fromByteStream huffmanDecodeInRequest  e33b `shouldBe` e33
            fromByteStream huffmanDecodeInResponse e51b `shouldBe` e51
            fromByteStream huffmanDecodeInResponse e52b `shouldBe` e52
            fromByteStream huffmanDecodeInResponse e53b `shouldBe` e53
