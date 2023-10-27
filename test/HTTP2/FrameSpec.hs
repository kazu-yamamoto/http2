{-# LANGUAGE OverloadedStrings #-}

module HTTP2.FrameSpec where

import Test.Hspec

import Data.ByteString.Char8 ()
import Data.Either
import Network.HTTP2.Frame

spec :: Spec
spec = do
    describe "encodeFrameHeader & decodeFrameHeader" $ do
        it "encode/decodes frames properly" $ do
            let header =
                    FrameHeader
                        { payloadLength = 500
                        , flags = defaultFlags
                        , streamId = 10
                        }
                wire = encodeFrameHeader FramePriority header
                fibHeader = decodeFrameHeader wire
            fibHeader `shouldBe` (FramePriority, header)

    describe "encodeFrame & decodeFrame" $ do
        it "encode/decodes frames properly" $ do
            let einfo =
                    EncodeInfo
                        { encodeFlags = defaultFlags
                        , encodeStreamId = 2
                        , encodePadding = Nothing
                        }
                payload = DataFrame "Hello, world!"
                wire = encodeFrame einfo payload
                frame =
                    fromRight (error "encode/decodes frames properly") $
                        decodeFrame defaultSettings wire
                payload' = framePayload frame
            payload' `shouldBe` payload
        it "encode/decodes padded frames properly" $ do
            let einfo =
                    EncodeInfo
                        { encodeFlags = defaultFlags
                        , encodeStreamId = 2
                        , encodePadding = Just "padding!"
                        }
                payload = DataFrame "Hello, world!"
                wire = encodeFrame einfo payload
                frame =
                    fromRight (error "encode/decodes padded frames properly") $
                        decodeFrame defaultSettings wire
                payload' = framePayload frame
            payload' `shouldBe` payload
