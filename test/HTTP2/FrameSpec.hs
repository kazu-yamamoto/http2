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
                ftyp = fromFrameTypeId FramePriority
                wire = encodeFrameHeader ftyp header
                Right fibHeader = decodeFrameHeader defaultSettings wire
            fibHeader `shouldBe` (ftyp, header)

    describe "encodeFrame & decodeFrame" $ do
        it "encode/decodes frames properly" $ do
            let einfo = EncodeInfo {
                    encodeFlags = defaultFlags
                  , encodeStreamId = StreamIdentifier 2
                  , encodePadding = Nothing
                  }
                payload = DataFrame "Hello, world!"
                wire = encodeFrame einfo payload
                Right frame = decodeFrame defaultSettings wire
                payload' = framePayload frame
            payload' `shouldBe` payload
        it "encode/decodes padded frames properly" $ do
            let einfo = EncodeInfo {
                    encodeFlags = defaultFlags
                  , encodeStreamId = StreamIdentifier 2
                  , encodePadding = Just "padding!"
                  }
                payload = DataFrame "Hello, world!"
                wire = encodeFrame einfo payload
                Right frame = decodeFrame defaultSettings wire
                payload' = framePayload frame
            payload' `shouldBe` payload
