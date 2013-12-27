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
    describe "toByteStream & fromByteStream" $ do
        prop "duality for request" $ \k v -> do
            let key = BS.pack ('k':k)
                val = BS.pack ('v':v)
                hb = [Literal Add (Lit key) val]
            fromByteStream huffmanDecodeInRequest
                (toByteStream huffmanEncodeInRequest hb) `shouldBe` hb
        prop "duality for response" $ \v -> do
            let val = BS.pack ('v':v)
                hb = [Literal Add (Idx 3) val]
            fromByteStream huffmanDecodeInResponse
                (toByteStream huffmanEncodeInResponse hb) `shouldBe` hb
