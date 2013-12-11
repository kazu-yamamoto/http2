module HuffmanRequestSpec where

import Data.Char
import Network.HPACK.Huffman.Request
import Numeric
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Printf

shouldBeEncoded :: String -> String -> Expectation
shouldBeEncoded inp out = (toS . huffmanEncodeInRequest . toW) inp `shouldBe` out
  where
    toW = map (fromIntegral . ord)
    toS = concatMap (printf "%02x")

shouldBeDecoded :: String -> String -> Expectation
shouldBeDecoded inp out = (toS . huffmanDecodeInRequest . toW) inp `shouldBe` out
  where
    toW = map fromHex . group2
    fromHex = fst . head . readHex
    group2 [] = []
    group2 xs = ys : group2 zs
      where
       (ys,zs) = splitAt 2 xs
    toS = map (chr . fromIntegral)

spec :: Spec
spec = do
    describe "huffmanEncodeInRequest and huffmanDecodeInRequest" $ do
        prop "duality" $ \ns ->
            let is = map ((`mod` 255) . abs) ns
            in huffmanDecodeInRequest (huffmanEncodeInRequest is) == is
    describe "huffmanEncodeInRequest" $ do
        it "encodes in request" $ do
            "www.example.com" `shouldBeEncoded` "db6d883e68d1cb1225ba7f"
            "no-cache" `shouldBeEncoded` "63654a1398ff"
            "custom-key" `shouldBeEncoded` "4eb08b749790fa7f"
            "custom-value" `shouldBeEncoded` "4eb08b74979a17a8ff"
    describe "huffmanDecodeInRequest" $ do
        it "decodes in request" $ do
            "db6d883e68d1cb1225ba7f" `shouldBeDecoded` "www.example.com"
            "63654a1398ff" `shouldBeDecoded` "no-cache"
            "4eb08b749790fa7f" `shouldBeDecoded` "custom-key"
            "4eb08b74979a17a8ff" `shouldBeDecoded` "custom-value"
