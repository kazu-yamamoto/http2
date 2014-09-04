{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Encode (
    encodeFrame
  , encodeFrameHeader
  , encodeFramePayload
  , buildFrame
  , buildFrameHeader
  , buildFramePayload
  ) where

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as BB
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Monoid ((<>))

import Network.HTTP2.Types

----------------------------------------------------------------

encodeFrame :: Frame -> ByteString
encodeFrame = run . buildFrame

encodeFrameHeader :: FrameHeader -> ByteString
encodeFrameHeader = run . buildFrameHeader

encodeFramePayload :: FramePayload -> ByteString
encodeFramePayload = run . buildFramePayload

run :: Builder -> ByteString
run = BL.toStrict . BB.toLazyByteString

----------------------------------------------------------------

buildFrame :: Frame -> Builder
buildFrame Frame{..} = buildFrameHeader frameHeader
                    <> buildFramePayload framePayload

----------------------------------------------------------------

buildFrameHeader :: FrameHeader -> Builder
buildFrameHeader FrameHeader{..} = len <> typ <> flags <> streamId
  where
    -- fixme: 2^14 check
    len1 = BB.fromWord16be (fromIntegral (payloadLength `shiftR` 8))
    len2 = BB.fromWord8 (fromIntegral (payloadLength .&. 0xff))
    len = len1 <> len2
    typ = BB.fromWord8 $ frameTypeToWord8 frameType
    flags = BB.fromWord8 fhFlags
    streamId = BB.fromWord32be $ fromStreamIdentifier fhStreamId

----------------------------------------------------------------

buildFramePayload :: FramePayload -> Builder

-- fixme: padding
buildFramePayload (DataFrame body) = BB.fromByteString body

buildFramePayload (HeaderFrame _ _ _ _) = undefined
buildFramePayload (PriorityFrame _ _ _) = undefined
buildFramePayload (RSTStreamFrame _) = undefined
buildFramePayload (SettingsFrame _) = undefined
buildFramePayload (PushPromiseFrame _ _) = undefined
buildFramePayload (PingFrame _) = undefined
buildFramePayload (GoAwayFrame _ _ _) = undefined
buildFramePayload (WindowUpdateFrame _) = undefined
buildFramePayload (ContinuationFrame _) = undefined
