module HuffmanRequestSpec where

import Control.Applicative ((<$>))
import Data.Char
import Network.HPACK.Huffman.Request
import Network.HPACK.Types
import Test.Hspec
import Test.Hspec.QuickCheck

import HexString

shouldBeEncoded :: String -> String -> Expectation
shouldBeEncoded inp out = enc inp `shouldBe` out
  where
    enc = toHexString . huffmanEncodeInRequest . toW
    toW = map (fromIntegral . ord)

shouldBeDecoded :: String -> Either DecodeError String -> Expectation
shouldBeDecoded inp out = dec inp `shouldBe` out
  where
    dec x = toS <$> huffmanDecodeInRequest (fromHexString x)
    toS = map (chr . fromIntegral)

spec :: Spec
spec = do
    describe "huffmanEncodeInRequest and huffmanDecodeInRequest" $ do
        prop "duality" $ \ns ->
            let is = map ((`mod` 255) . abs) ns
            in huffmanDecodeInRequest (huffmanEncodeInRequest is) == Right is
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
