module HuffmanResponseSpec where

import Data.Char
import Network.HPACK.Huffman.Response
import Test.Hspec
import Test.Hspec.QuickCheck

import HexString

shouldBeEncoded :: String -> String -> Expectation
shouldBeEncoded inp out = enc inp `shouldBe` out
  where
    enc = toHexString . huffmanEncodeInResponse . toW
    toW = map (fromIntegral . ord)

shouldBeDecoded :: String -> String -> Expectation
shouldBeDecoded inp out = dec inp `shouldBe` out
  where
    dec = toS . huffmanDecodeInResponse . fromHexString
    toS = map (chr . fromIntegral)

spec :: Spec
spec = do
    describe "huffmanEncodeInResponse and huffmanDecodeInResponse" $ do
        prop "duality" $ \ns ->
            let is = map ((`mod` 255) . abs) ns
            in huffmanDecodeInResponse (huffmanEncodeInResponse is) == is
    describe "huffmanEncodeInResponse" $ do
        it "encodes in response" $ do
            "private" `shouldBeEncoded` "c31b39bf387f"
            "Mon, 21 Oct 2013 20:13:21 GMT" `shouldBeEncoded` "a2fba20320f2ab303124018b490d3209e877"
            "https://www.example.com" `shouldBeEncoded` "e39e7864dd7afd3d3d248747db87284955f6ff"
            "Mon, 21 Oct 2013 20:13:22 GMT" `shouldBeEncoded` "a2fba20320f2ab303124018b490d3309e877"
            "gzip" `shouldBeEncoded` "e1fbb30f"
            "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1" `shouldBeEncoded` "df7dfb36d3d9e1fcfc3fafe7abfcfefcbfaf3edf2f977fd36ff7fd79f6f977fd3de16bfa46fe10d889447de1ce18e565f76c2f"
    describe "huffmanDecodeInResponse" $ do
        it "decodes in response" $ do
            "c31b39bf387f" `shouldBeDecoded` "private"
            "a2fba20320f2ab303124018b490d3209e877"`shouldBeDecoded` "Mon, 21 Oct 2013 20:13:21 GMT"
            "e39e7864dd7afd3d3d248747db87284955f6ff" `shouldBeDecoded` "https://www.example.com"
            "a2fba20320f2ab303124018b490d3309e877" `shouldBeDecoded` "Mon, 21 Oct 2013 20:13:22 GMT"
            "e1fbb30f" `shouldBeDecoded` "gzip"
            "df7dfb36d3d9e1fcfc3fafe7abfcfefcbfaf3edf2f977fd36ff7fd79f6f977fd3de16bfa46fe10d889447de1ce18e565f76c2f" `shouldBeDecoded` "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"
