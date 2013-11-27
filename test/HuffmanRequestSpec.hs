module HuffmanRequestSpec where

import Data.Char
import Network.HPACK.HuffmanRequest
import Numeric
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Printf

shouldBeEncoded :: String -> String -> Expectation
shouldBeEncoded inp out = (toS . encodeInRequest . toI) inp `shouldBe` out
  where
    toI = map ord
    toS = concatMap (printf "%02x")

shouldBeDecoded :: String -> String -> Expectation
shouldBeDecoded inp out = (toS . decodeInRequest . toI) inp `shouldBe` out
  where
    toI = map fromHex . group2
    fromHex = fst . head . readHex
    group2 [] = []
    group2 xs = ys : group2 zs
      where
       (ys,zs) = splitAt 2 xs
    toS = map chr

spec :: Spec
spec = do
    describe "encodeInRequest and decodeInRequest" $ do
        prop "duality" $ \ns ->
            let is = map ((`mod` 255) . abs) ns
            in decodeInRequest (encodeInRequest is) == is
    describe "encodeInRequest" $ do
        it "encodes in request" $ do
            "www.example.com" `shouldBeEncoded` "db6d883e68d1cb1225ba7f"
            "no-cache" `shouldBeEncoded` "63654a1398ff"
            "custom-key" `shouldBeEncoded` "4eb08b749790fa7f"
            "custom-value" `shouldBeEncoded` "4eb08b74979a17a8ff"
    describe "decodeInRequest" $ do
        it "decodes in request" $ do
            "db6d883e68d1cb1225ba7f" `shouldBeDecoded` "www.example.com"
            "63654a1398ff" `shouldBeDecoded` "no-cache"
            "4eb08b749790fa7f" `shouldBeDecoded` "custom-key"
            "4eb08b74979a17a8ff" `shouldBeDecoded` "custom-value"
