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
buildFrameHeader FrameHeader{..} = len <> typ <> flg <> sid
  where
    -- fixme: 2^14 check
    len1 = BB.fromWord16be (fromIntegral (payloadLength `shiftR` 8))
    len2 = BB.fromWord8 (fromIntegral (payloadLength .&. 0xff))
    len = len1 <> len2
    typ = BB.fromWord8 $ frameTypeToWord8 frameType
    flg = BB.fromWord8 flags
    sid = BB.fromWord32be $ fromStreamIdentifier streamId

----------------------------------------------------------------

buildFramePayload :: FramePayload -> Builder

-- fixme: padding
buildFramePayload (DataFrame body) = BB.fromByteString body

-- fixme: padding
buildFramePayload (HeaderFrame (Just p) hdr) = buildPriority p <> BB.fromByteString hdr
buildFramePayload (HeaderFrame Nothing hdr) = BB.fromByteString hdr

buildFramePayload (PriorityFrame p) = buildPriority p

buildFramePayload (RSTStreamFrame e) = buildErrorCode e

buildFramePayload (SettingsFrame _) = undefined
buildFramePayload (PushPromiseFrame _ _) = undefined

buildFramePayload (PingFrame bs) = BB.fromByteString bs

buildFramePayload (GoAwayFrame sid e bs) =
    buildStreamIdentifier sid <> buildErrorCode e <> BB.fromByteString bs

buildFramePayload (WindowUpdateFrame _) = undefined

buildFramePayload (ContinuationFrame hdr) = BB.fromByteString hdr

buildPriority :: Priority -> Builder
buildPriority = undefined

buildErrorCode :: WErrorCode -> Builder
buildErrorCode (Right e) = undefined
buildErrorCode (Left e) = undefined

buildStreamIdentifier :: StreamIdentifier -> Builder
buildStreamIdentifier = undefined
