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
  , ("www.example.com", "e7cf9bebe89b6fb16fa9b6ff")
  , ("no-cache", "b9b9949556bf")
  , ("custom-key", "571c5cdb737b2faf")
  , ("custom-value", "571c5cdb73724d9c57")
  , ("private", "bf06724b97")
  , ("Mon, 21 Oct 2013 20:13:21 GMT", "d6dbb29884de2a718805062098513109b56ba3")
  , ("https://www.example.com", "adcebf198e7e7cf9bebe89b6fb16fa9b6f")
  , ("Mon, 21 Oct 2013 20:13:22 GMT", "d6dbb29884de2a718805062098513111b56ba3")
  , ("gzip", "abdd97ff")
  , ("foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1", "e0d6cf9f6e8f9fd3e5f6fa76fefd3c7edf9eff1f2f0f3cfe9f6fcf7f8f879f61ad4f4cc9a973a2200ec3725e18b1b74e3f")
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
            "ffffeeff" `shouldBeDecoded` Right "\1"
            "ffffeeffff" `shouldBeDecoded` Left TooLongEos
            mapM_ (\(x,y) -> y `shouldBeDecoded` Right x) testData
