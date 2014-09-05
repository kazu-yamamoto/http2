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
import Data.Monoid ((<>))

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
    (frameTypeToWord8 FrameData, buildFramePayloadData einfo body)
buildFramePayload einfo (HeadersFrame mpri hdr) =
    (frameTypeToWord8 FrameHeaders, buildFramePayloadHeaders einfo mpri hdr)
buildFramePayload _ _ = undefined

----------------------------------------------------------------

buildPadding :: EncodeInfo -> Builder -> PayloadLength -> (FrameHeader, Builder)
buildPadding EncodeInfo{ encodePadding = Nothing, ..} builder len =
    (header, builder)
  where
    header = FrameHeader len encodeFlags encodeStreamId
buildPadding EncodeInfo{ encodePadding = Just padding, ..} btarget targetLength =
    (header, builder)
  where
    header = FrameHeader len newflags encodeStreamId
    builder = bpadlen <> btarget <> bpadding
    bpadlen = BB.fromWord8 $ fromIntegral paddingLength
    bpadding = BB.fromByteString padding
    paddingLength = B.length padding
    len = targetLength + paddingLength + 1
    newflags = setPadded encodeFlags

buildPriority :: Priority -> Builder
buildPriority Priority{..} = builder
  where
    builder = bstream <> bweight
    stream = fromStreamIdentifier streamDependency
    estream
      | exclusive = setExclusive stream
      | otherwise = stream
    bstream = BB.fromWord32be estream
    bweight = BB.fromWord8 $ fromIntegral $ weight - 1

----------------------------------------------------------------

buildFramePayloadData :: EncodeInfo -> ByteString -> (FrameHeader, Builder)
buildFramePayloadData einfo body = buildPadding einfo builder len
  where
    builder = BB.fromByteString body
    len = B.length body

buildFramePayloadHeaders :: EncodeInfo -> Maybe Priority -> ByteString
                         -> (FrameHeader, Builder)
buildFramePayloadHeaders einfo Nothing hdr =
    buildPadding einfo builder len
  where
    builder = BB.fromByteString hdr
    len = B.length hdr
buildFramePayloadHeaders einfo (Just pri) hdr =
    buildPadding einfo builder len
  where
    builder = buildPriority pri <> BB.fromByteString hdr
    len = B.length hdr + 5

{-
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

buildErrorCode :: ErrorCode -> Builder
buildErrorCode = undefined

buildStreamIdentifier :: StreamIdentifier -> Builder
buildStreamIdentifier = undefined
-}
