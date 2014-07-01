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
  , ("www.example.com", "f1e3c2e5f23a6ba0ab90f4ff")
  , ("no-cache", "a8eb10649cbf")
  , ("custom-key", "25a849e95ba97d7f")
  , ("custom-value", "25a849e95bb8e8b4bf")
  , ("private", "aec3771a4b")
  , ("Mon, 21 Oct 2013 20:13:21 GMT", "d07abe941054d444a8200595040b8166e082a62d1bff")
  , ("https://www.example.com", "9d29ad171863c78f0b97c8e9ae82ae43d3")
  , ("Mon, 21 Oct 2013 20:13:22 GMT", "d07abe941054d444a8200595040b8166e084a62d1bff")
  , ("gzip", "9bd9ab")
  , ("foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1", "94e7821dd7f2e6c7b335dfdfcd5b3960d5af27087f3672c1ab270fb5291f9587316065c003ed4ee5b1063d5007")
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
            "ffffea" `shouldBeDecoded` Right "\9"
            "ffffeaff" `shouldBeDecoded` Left TooLongEos
            mapM_ (\(x,y) -> y `shouldBeDecoded` Right x) testData
