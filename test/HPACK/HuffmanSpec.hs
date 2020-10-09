{-# LANGUAGE OverloadedStrings, CPP #-}

module HPACK.HuffmanSpec where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import Network.HPACK
import Network.HPACK.Huffman
import Test.Hspec
import Test.Hspec.QuickCheck

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
shouldBeEncoded inp out = do
    out' <- BS.map toLower . B16.encode <$> encodeHuffman inp
    out' `shouldBe` out

shouldBeDecoded :: ByteString -> ByteString -> Expectation
shouldBeDecoded inp out = do
    out' <- decodeHuffman $ B16.decodeLenient inp
    out' `shouldBe` out

tryDecode :: ByteString -> IO ByteString
tryDecode inp = decodeHuffman $ B16.decodeLenient inp

spec :: Spec
spec = do
    describe "encode and decode" $ do
        prop "duality" $ \cs -> do
            let bs = BS.pack cs
            es <- encodeHuffman bs
            ds <- decodeHuffman es
            ds `shouldBe` bs
    describe "encode" $ do
        it "encodes" $ do
            mapM_ (\(x,y) -> x `shouldBeEncoded` y) testData
    describe "decode" $ do
        it "decodes" $ do
            tryDecode       "ff" `shouldThrow` (== TooLongEos)
            tryDecode "ffffeaff" `shouldThrow` (== TooLongEos)
            "ffffea" `shouldBeDecoded` "\9"
            mapM_ (\(x,y) -> y `shouldBeDecoded` x) testData
