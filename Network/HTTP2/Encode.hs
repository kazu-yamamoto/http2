{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Encode (
    encodeFrame
  ) where

import Blaze.ByteString.Builder
import Data.Monoid ((<>))
import Data.Bits

import Network.HTTP2.Types

encodeFrame :: Frame -> Builder
encodeFrame = undefined

encodeFrameHeader :: FrameHeader -> Builder
encodeFrameHeader FrameHeader{..} = len <> typ <> flags <> streamId
  where
    -- fixme: 2^14 check
    len1 = fromWord16be (fromIntegral (fhLength `shiftR` 8))
    len2 = fromWord8 (fromIntegral (fhLength .&. 0xff))
    len = len1 <> len2
    typ = fromWord8 $ frameTypeToWord8 fhType
    flags = fromWord8 fhFlags
    streamId = fromWord32be $ fromStreamIdentifier fhStreamId

encodeFramePayload :: FramePayload-> Builder
encodeFramePayload = undefined
