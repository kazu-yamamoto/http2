{-# LANGUAGE OverloadedStrings #-}

module HTTP2.FrameSpec where

import Test.Hspec

import qualified Data.ByteString as BS
import Data.ByteString.Char8 ()
import Data.Either
import Network.HTTP2.Frame

-- | The error a decoder reports, or Nothing when it accepted the payload.
decodeError :: FrameType -> FrameHeader -> BS.ByteString -> Maybe ErrorCode
decodeError typ header body = case decodeFramePayload typ header body of
    Left (FrameDecodeError ec _ _) -> Just ec
    Right _ -> Nothing

spec :: Spec
spec = do
    describe "decodeFramePayload" $ do
        -- Each of these used to reach a peek at a fixed offset that never
        -- consulted the length of the ByteString it was reading from.  An
        -- empty one is the shared empty ByteString, whose pointer is null, so
        -- the result was a segfault rather than an exception -- which is why
        -- none of this could be written as a failing assertion before.
        it "rejects a padded frame with no room for Pad Length" $ do
            let padded = FrameHeader 0 (setPadded defaultFlags) 1
            decodeError FrameData padded "" `shouldBe` Just FrameSizeError
            decodeError FramePushPromise padded "" `shouldBe` Just FrameSizeError

        it "rejects a padded HEADERS whose padding covers the priority fields" $ do
            -- Six octets is the smallest payload the header check accepts for
            -- PADDED and PRIORITY together, and a Pad Length of five leaves
            -- none of the five priority octets behind.
            let flags = setPadded $ setPriority defaultFlags
                header = FrameHeader 6 flags 1
            decodeError FrameHeaders header (BS.pack [5, 0, 0, 0, 0, 0])
                `shouldBe` Just FrameSizeError

        it "rejects a padded PUSH_PROMISE whose padding covers the promised id" $ do
            let flags = setPadded defaultFlags
                header = FrameHeader 5 flags 1
            decodeError FramePushPromise header (BS.pack [4, 0, 0, 0, 0])
                `shouldBe` Just FrameSizeError

        it "rejects a payload shorter than the frame header promised" $ do
            -- What a peer that hangs up mid-frame leaves behind.
            decodeError FramePriority (FrameHeader 5 defaultFlags 1) ""
                `shouldBe` Just FrameSizeError
            decodeError FrameRSTStream (FrameHeader 4 defaultFlags 1) ""
                `shouldBe` Just FrameSizeError
            decodeError FrameWindowUpdate (FrameHeader 4 defaultFlags 1) ""
                `shouldBe` Just FrameSizeError
            decodeError FrameSettings (FrameHeader 6 defaultFlags 0) ""
                `shouldBe` Just FrameSizeError

        it "rejects a payload too short for the fields it holds" $ do
            -- A payloadLength of zero satisfies checkFrameSize against an
            -- empty payload, but each of these still has a fixed-size field
            -- to read.  GOAWAY was a segfault; the other three quietly
            -- returned whatever lay past the end of the buffer.
            let lying = FrameHeader 0 defaultFlags 1
            decodeError FrameRSTStream lying "" `shouldBe` Just FrameSizeError
            decodeError FrameWindowUpdate lying "" `shouldBe` Just FrameSizeError
            decodeError FramePriority lying "" `shouldBe` Just FrameSizeError
            decodeError FrameGoAway lying "" `shouldBe` Just FrameSizeError

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
                        decodeFrame wire
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
                        decodeFrame wire
                payload' = framePayload frame
            payload' `shouldBe` payload
