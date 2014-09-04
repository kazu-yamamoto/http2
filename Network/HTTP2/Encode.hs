{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Encode (
    encodeFrame
  , encodeFrameHeader
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

encodeFrame :: FrameHeader -> ByteString
encodeFrame = undefined

encodeFrameHeader :: FrameHeader -> ByteString
encodeFrameHeader frame = BL.toStrict $ BB.toLazyByteString $ buildFrameHeader frame

buildFrame :: Frame -> Builder
buildFrame = undefined

buildFrameHeader :: FrameHeader -> Builder
buildFrameHeader FrameHeader{..} = len <> typ <> flags <> streamId
  where
    -- fixme: 2^14 check
    len1 = BB.fromWord16be (fromIntegral (fhLength `shiftR` 8))
    len2 = BB.fromWord8 (fromIntegral (fhLength .&. 0xff))
    len = len1 <> len2
    typ = BB.fromWord8 $ frameTypeToWord8 fhType
    flags = BB.fromWord8 fhFlags
    streamId = BB.fromWord32be $ fromStreamIdentifier fhStreamId

buildFramePayload :: FramePayload-> Builder
buildFramePayload = undefined
