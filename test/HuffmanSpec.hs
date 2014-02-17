{-# LANGUAGE OverloadedStrings #-}

module HuffmanSpec where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.HPACK
import Network.HPACK.Huffman
import Test.Hspec
import Test.Hspec.QuickCheck

import HexString

testData :: [(ByteString, ByteString)]
testData = [
    ("", "")
  , ("www.example.com", "db6d883e68d1cb1225ba7f")
  , ("no-cache", "63654a1398ff")
  , ("custom-key", "4eb08b749790fa7f")
  , ("custom-value", "4eb08b74979a17a8ff")
  , ("private", "73d5cd111f")
  , ("Mon, 21 Oct 2013 20:13:21 GMT", "ef6b3a7a0e6e8fa263d0729a6e8397d869bd873747bbbfc7")
  , ("https://www.example.com", "ce31743d801b6db107cd1a396244b74f")
  , ("Mon, 21 Oct 2013 20:13:22 GMT", "ef6b3a7a0e6e8fa263d0729a6e8397d869bd873f47bbbfc7")
  , ("gzip", "cbd54e")
  , ("foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1", "c5adb77f876fc7fbf7fdbfbebff3f7f4fb7ebbbe9f5f87e37fefedfaeefa7c3f1d5d1a23ce546436cd494bd5d1cc5f0535969b")
  ]

shouldBeEncoded :: ByteString -> ByteString -> Expectation
shouldBeEncoded inp out = enc inp `shouldBe` out
  where
    enc = toHexString . encode

shouldBeDecoded :: ByteString -> Either DecodeError ByteString -> Expectation
shouldBeDecoded inp out = dec inp `shouldBe` out
  where
    dec = decode . fromHexString

spec :: Spec
spec = do
    describe "encode and decode" $ do
        prop "duality" $ \cs ->
            let bs = BS.pack cs
            in decode (encode bs) == Right bs
    describe "encode" $ do
        it "encodes" $ do
            mapM_ (\(x,y) -> x `shouldBeEncoded` y) testData
    describe "decode" $ do
        it "decodes" $ do
            "ff" `shouldBeDecoded` Left TooLongEos
            "fffff77f" `shouldBeDecoded` Right "\1"
            "fffff77fff" `shouldBeDecoded` Left TooLongEos
            mapM_ (\(x,y) -> y `shouldBeDecoded` Right x) testData
