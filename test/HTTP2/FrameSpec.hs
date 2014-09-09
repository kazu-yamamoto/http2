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
            let header = FrameHeader {
                    payloadLength = 500
                  , flags = defaultFlags
                  , streamId = StreamIdentifier 10
                  }
                fid = frameTypeToWord8 FramePriority
                wire = encodeFrameHeader fid header
                eheader = decodeFrameHeader defaultSettings wire
            eheader `shouldBe` Right (fid, header)

    describe "encodeFrame & decodeFrame" $ do
        it "encode/decodes frames properly" $ do
            let einfo = EncodeInfo 0 (StreamIdentifier 2) Nothing
                payload = DataFrame "Hello, world!"
                Right frame = decodeFrame defaultSettings $ encodeFrame einfo payload
            framePayload frame `shouldBe` payload
