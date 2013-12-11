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
            toByteStream huffmanEncodeInRequest e31 `shouldBe` e31b
            toByteStream huffmanEncodeInRequest e32 `shouldBe` e32b
            toByteStream huffmanEncodeInRequest e33 `shouldBe` e33b
            toByteStream huffmanEncodeInResponse e51 `shouldBe` e51b
            toByteStream huffmanEncodeInResponse e52 `shouldBe` e52b
            toByteStream huffmanEncodeInResponse e53 `shouldBe` e53b
