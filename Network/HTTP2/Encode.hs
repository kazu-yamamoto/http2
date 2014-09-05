{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Encode (
    encodeFrame
  , encodeFrameHeader
  , encodeFramePayload
  , buildFrame
  , buildFrameHeader
  , buildFramePayload
  , EncodeInfo(..)
  ) where

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as BB
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid ((<>), mempty)

import Network.HTTP2.Types

----------------------------------------------------------------

data EncodeInfo = EncodeInfo {
      encodeFlags    :: FrameFlags
    , encodeStreamId :: StreamIdentifier
    , encodePadding  :: Maybe Padding
    }

----------------------------------------------------------------

encodeFrame :: EncodeInfo -> FramePayload -> ByteString
encodeFrame einfo payload = run $ buildFrame einfo payload

encodeFrameHeader :: FrameTypeId -> FrameHeader -> ByteString
encodeFrameHeader fid header = run $ buildFrameHeader fid header

encodeFramePayload :: EncodeInfo -> FramePayload -> ByteString
encodeFramePayload einfo payload = run payloadBuilder
  where
    (_, (_, payloadBuilder)) = buildFramePayload einfo payload

run :: Builder -> ByteString
run = BL.toStrict . BB.toLazyByteString

----------------------------------------------------------------

buildFrame :: EncodeInfo -> FramePayload -> Builder
buildFrame einfo payload = headerBuilder <> payloadBuilder
  where
    (fid, (header, payloadBuilder)) = buildFramePayload einfo payload
    headerBuilder = buildFrameHeader fid header

----------------------------------------------------------------

buildFrameHeader :: FrameTypeId -> FrameHeader -> Builder
buildFrameHeader fid FrameHeader{..} = len <> typ <> flg <> sid
  where
    -- fixme: 2^14 check
    len1 = BB.fromWord16be (fromIntegral (payloadLength `shiftR` 8))
    len2 = BB.fromWord8 (fromIntegral (payloadLength .&. 0xff))
    len = len1 <> len2
    typ = BB.fromWord8 fid
    flg = BB.fromWord8 flags
    sid = BB.fromWord32be $ fromStreamIdentifier streamId

----------------------------------------------------------------

buildFramePayload :: EncodeInfo -> FramePayload
                  -> (FrameTypeId, (FrameHeader, Builder))
buildFramePayload einfo (DataFrame body) =
    (frameTypeToWord8 FrameData, buildFramPyloadData einfo body)
buildFramePayload _ _ = undefined

buildFramPyloadData :: EncodeInfo -> ByteString -> (FrameHeader, Builder)
buildFramPyloadData EncodeInfo{..} body = (header, plen <> dat <> padding)
  where
    dat = BB.fromByteString body
    blen = B.length body
    (len, plen, padding) = case encodePadding of
        Nothing  -> (blen, mempty, mempty)
        Just pad -> let padlen = B.length pad
                    in (blen + padlen + 1, BB.fromWord8 (fromIntegral padlen),
                        BB.fromByteString pad)
    flags = 0
    header = FrameHeader len flags encodeStreamId

{-
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
buildFramePayload (UnknownFrame _ _) = undefined

buildPriority :: Priority -> Builder
buildPriority = undefined

buildErrorCode :: ErrorCode -> Builder
buildErrorCode = undefined

buildStreamIdentifier :: StreamIdentifier -> Builder
buildStreamIdentifier = undefined
-}
