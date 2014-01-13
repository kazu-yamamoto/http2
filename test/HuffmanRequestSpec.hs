{-# LANGUAGE OverloadedStrings #-}

module HuffmanRequestSpec where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.HPACK.Huffman.Request
import Network.HPACK.Types
import Test.Hspec
import Test.Hspec.QuickCheck

import HexString

shouldBeEncoded :: ByteString -> ByteString -> Expectation
shouldBeEncoded inp out = enc inp `shouldBe` out
  where
    enc = toHexString . huffmanEncodeInRequest

shouldBeDecoded :: ByteString -> Either DecodeError ByteString -> Expectation
shouldBeDecoded inp out = dec inp `shouldBe` out
  where
    dec x = huffmanDecodeInRequest (fromHexString x)

spec :: Spec
spec = do
    describe "huffmanEncodeInRequest and huffmanDecodeInRequest" $ do
        prop "duality" $ \cs ->
            let bs = BS.pack cs
            in huffmanDecodeInRequest (huffmanEncodeInRequest bs) == Right bs
    describe "huffmanEncodeInRequest" $ do
        it "encodes in request" $ do
            "www.example.com" `shouldBeEncoded` "db6d883e68d1cb1225ba7f"
            "no-cache" `shouldBeEncoded` "63654a1398ff"
            "custom-key" `shouldBeEncoded` "4eb08b749790fa7f"
            "custom-value" `shouldBeEncoded` "4eb08b74979a17a8ff"
            "" `shouldBeEncoded` ""
    describe "huffmanDecodeInRequest" $ do
        it "decodes in request" $ do
            "db6d883e68d1cb1225ba7f" `shouldBeDecoded` Right "www.example.com"
            "63654a1398ff" `shouldBeDecoded` Right "no-cache"
            "4eb08b749790fa7f" `shouldBeDecoded` Right "custom-key"
            "4eb08b74979a17a8ff" `shouldBeDecoded` Right "custom-value"
            "ff" `shouldBeDecoded` Left TooLongEos
            "fffff77f" `shouldBeDecoded` Right "\1"
            "fffff77fff" `shouldBeDecoded` Left TooLongEos
