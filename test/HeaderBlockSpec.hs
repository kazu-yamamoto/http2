{-# LANGUAGE OverloadedStrings #-}

module HeaderBlockSpec where

import qualified Data.ByteString as BS
import Network.HPACK.HeaderBlock
import Network.HPACK.Huffman
import Network.HPACK.Types
import Test.Hspec

import HeaderBlock
import HexString

spec :: Spec
spec = do
    describe "toByteStream" $ do
        it "encodes HeaderBlock" $ do
            toByteStream huffmanEncodeInRequest e211 `shouldBe` re211
            toByteStream huffmanEncodeInRequest e212 `shouldBe` re212
            toByteStream huffmanEncodeInRequest e213 `shouldBe` re213

re211 :: ByteStream
re211 = BS.pack $ fromHexString "828786048bdb6d883e68d1cb1225ba7f"

re212 :: ByteStream
re212 = BS.pack $ fromHexString "1b8663654a1398ff"

re213 :: ByteStream
re213 = BS.pack $ fromHexString "80858c8b8400884eb08b749790fa7f894eb08b74979a17a8ff"
