{-# LANGUAGE OverloadedStrings #-}

module HTTP2.FrameSpec where

import Test.Hspec

import Data.ByteString.Char8 ()
import Network.HTTP2.Decode
import Network.HTTP2.Encode
import Network.HTTP2.Types

spec :: Spec
spec = do
    describe "encodeFrameHeader & decodeFrameHeader" $ do
        it "encode/decodes frames properly" $ do
            let header = FrameHeader 500 FrameData 0 (StreamIdentifier 10)
                eheader = decodeFrameHeader defaultSettings $ encodeFrameHeader header
            eheader `shouldBe` Right header

    describe "encodeFrame & decodeFrame" $ do
        it "encode/decodes frames properly" $ do
            let payload = DataFrame "Hello, world!"
                Right frame = decodeFrame defaultSettings $ encodeFrame payload 0 Nothing
            framePayload frame `shouldBe` payload
