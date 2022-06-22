{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Frame.Encode (
    encodeFrame
  , encodeFrameChunks
  , encodeFrameHeader
  , encodeFrameHeaderBuf
  , encodeFramePayload
  , EncodeInfo(..)
  , encodeInfo
  ) where

import qualified Data.ByteString as BS
import Data.ByteString.Internal (unsafeCreate)
import Foreign.Ptr (Ptr, plusPtr)
import qualified Network.ByteOrder as N

import Imports
import Network.HTTP2.Frame.Types

----------------------------------------------------------------

type Builder = [ByteString] -> [ByteString]

-- | Auxiliary information for frame encoding.
data EncodeInfo = EncodeInfo {
    -- | Flags to be set in a frame header
      encodeFlags    :: FrameFlags
    -- | Stream id to be set in a frame header
    , encodeStreamId :: StreamId
    -- | Padding if any. In the case where this value is set but the priority flag is not set, this value gets preference over the priority flag. So, if this value is set, the priority flag is also set.
    , encodePadding  :: Maybe Padding
    } deriving (Show,Read)

----------------------------------------------------------------

-- | A smart builder of 'EncodeInfo'.
--
-- >>> encodeInfo setAck 0
-- EncodeInfo {encodeFlags = 1, encodeStreamId = 0, encodePadding = Nothing}
encodeInfo :: (FrameFlags -> FrameFlags)
           -> Int -- ^ stream identifier
           -> EncodeInfo
encodeInfo set sid = EncodeInfo (set defaultFlags) sid Nothing

----------------------------------------------------------------

-- | Encoding an HTTP/2 frame to 'ByteString'.
-- This function is not efficient enough for high performace
-- program because of the concatenation of 'ByteString'.
--
-- >>> encodeFrame (encodeInfo id 1) (DataFrame "body")
-- "\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\SOHbody"
encodeFrame :: EncodeInfo -> FramePayload -> ByteString
encodeFrame einfo payload = BS.concat $ encodeFrameChunks einfo payload

-- | Encoding an HTTP/2 frame to ['ByteString'].
--   This is suitable for sendMany.
encodeFrameChunks :: EncodeInfo -> FramePayload -> [ByteString]
encodeFrameChunks einfo payload = bs : bss
  where
    ftid = framePayloadToFrameType payload
    bs = encodeFrameHeader ftid header
    (header, bss) = encodeFramePayload einfo payload

-- | Encoding an HTTP/2 frame header.
--   The frame header must be completed.
encodeFrameHeader :: FrameType -> FrameHeader -> ByteString
encodeFrameHeader ftid fhdr = unsafeCreate frameHeaderLength $ encodeFrameHeaderBuf ftid fhdr

-- | Writing an encoded HTTP/2 frame header to the buffer.
--   The length of the buffer must be larger than or equal to 9 bytes.
encodeFrameHeaderBuf :: FrameType -> FrameHeader -> Ptr Word8 -> IO ()
encodeFrameHeaderBuf ftid FrameHeader{..} ptr = do
    N.poke24 plen  ptr 0
    N.poke8  typ   ptr 3
    N.poke8  flags ptr 4
    N.poke32 sid   ptr 5
  where
    plen = fromIntegral payloadLength
    typ = fromFrameType ftid
    sid = fromIntegral streamId

-- | Encoding an HTTP/2 frame payload.
--   This returns a complete frame header and chunks of payload.
encodeFramePayload :: EncodeInfo -> FramePayload -> (FrameHeader, [ByteString])
encodeFramePayload einfo payload = (header, builder [])
  where
    (header, builder) = buildFramePayload einfo payload

----------------------------------------------------------------

buildFramePayload :: EncodeInfo -> FramePayload -> (FrameHeader, Builder)
buildFramePayload einfo (DataFrame body) =
    buildFramePayloadData einfo body
buildFramePayload einfo (HeadersFrame mpri hdr) =
    buildFramePayloadHeaders einfo mpri hdr
buildFramePayload einfo (PriorityFrame pri) =
    buildFramePayloadPriority einfo pri
buildFramePayload einfo (RSTStreamFrame e) =
    buildFramePayloadRSTStream einfo e
buildFramePayload einfo (SettingsFrame settings) =
    buildFramePayloadSettings einfo settings
buildFramePayload einfo (PushPromiseFrame sid hdr) =
    buildFramePayloadPushPromise einfo sid hdr
buildFramePayload einfo (PingFrame opaque) =
    buildFramePayloadPing einfo opaque
buildFramePayload einfo (GoAwayFrame sid e debug) =
    buildFramePayloadGoAway einfo sid e debug
buildFramePayload einfo (WindowUpdateFrame size) =
    buildFramePayloadWindowUpdate einfo size
buildFramePayload einfo (ContinuationFrame hdr) =
    buildFramePayloadContinuation einfo hdr
buildFramePayload einfo (UnknownFrame _ opaque) =
    buildFramePayloadUnknown einfo opaque

----------------------------------------------------------------

buildPadding :: EncodeInfo
             -> Builder
             -> Int -- ^ Payload length.
             -> (FrameHeader, Builder)
buildPadding EncodeInfo{ encodePadding = Nothing, ..} builder len =
    (header, builder)
  where
    header = FrameHeader len encodeFlags encodeStreamId
buildPadding EncodeInfo{ encodePadding = Just padding, ..} btarget targetLength =
    (header, builder)
  where
    header = FrameHeader len newflags encodeStreamId
    builder = (b1 :) . btarget . (padding :)
    b1 = BS.singleton $ fromIntegral paddingLength
    paddingLength = BS.length padding
    len = targetLength + paddingLength + 1
    newflags = setPadded encodeFlags

buildPriority :: Priority -> Builder
buildPriority Priority{..} = builder
  where
    builder = (priority :)
    estream
      | exclusive = setExclusive streamDependency
      | otherwise = streamDependency
    priority = unsafeCreate 5 $ \ptr -> do
        let esid = fromIntegral estream
            w    = fromIntegral $ weight - 1
        N.poke32 esid ptr 0
        N.poke8  w    ptr 4

----------------------------------------------------------------

buildFramePayloadData :: EncodeInfo -> ByteString -> (FrameHeader, Builder)
buildFramePayloadData einfo body = buildPadding einfo builder len
  where
    builder = (body :)
    len = BS.length body

buildFramePayloadHeaders :: EncodeInfo -> Maybe Priority -> HeaderBlockFragment
                         -> (FrameHeader, Builder)
buildFramePayloadHeaders einfo Nothing hdr =
    buildPadding einfo builder len
  where
    builder = (hdr :)
    len = BS.length hdr
buildFramePayloadHeaders einfo (Just pri) hdr =
    buildPadding einfo' builder len
  where
    builder = buildPriority pri . (hdr :)
    len = BS.length hdr + 5
    einfo' = einfo { encodeFlags = setPriority (encodeFlags einfo) }

buildFramePayloadPriority :: EncodeInfo -> Priority -> (FrameHeader, Builder)
buildFramePayloadPriority EncodeInfo{..} p = (header, builder)
  where
    builder = buildPriority p
    header = FrameHeader 5 encodeFlags encodeStreamId

buildFramePayloadRSTStream :: EncodeInfo -> ErrorCode -> (FrameHeader, Builder)
buildFramePayloadRSTStream EncodeInfo{..} e = (header, builder)
  where
    builder = (b4 :)
    b4 = N.bytestring32 $ fromErrorCode e
    header = FrameHeader 4 encodeFlags encodeStreamId

buildFramePayloadSettings :: EncodeInfo -> SettingsList -> (FrameHeader, Builder)
buildFramePayloadSettings EncodeInfo{..} alist = (header, builder)
  where
    builder = (settings :)
    settings = unsafeCreate len $ \ptr -> go ptr alist
    go _ []          = return ()
    go p ((k,v):kvs) = do
        N.poke16 (fromSettingsKeyId k) p 0
        N.poke32 (fromIntegral v)      p 2
        go (p `plusPtr` 6) kvs
    len = length alist * 6
    header = FrameHeader len encodeFlags encodeStreamId

buildFramePayloadPushPromise :: EncodeInfo -> StreamId -> HeaderBlockFragment -> (FrameHeader, Builder)
buildFramePayloadPushPromise einfo sid hdr = buildPadding einfo builder len
  where
    builder = (b4 :) . (hdr :)
    b4 = N.bytestring32 $ fromIntegral sid
    len = 4 + BS.length hdr

buildFramePayloadPing :: EncodeInfo -> ByteString -> (FrameHeader, Builder)
buildFramePayloadPing EncodeInfo{..} odata = (header, builder)
  where
    builder = (odata :)
    header = FrameHeader 8 encodeFlags encodeStreamId

buildFramePayloadGoAway :: EncodeInfo -> StreamId -> ErrorCode -> ByteString -> (FrameHeader, Builder)
buildFramePayloadGoAway EncodeInfo{..} sid e debug = (header, builder)
  where
    builder = (b8 :) . (debug :)
    len0 = 8
    b8 = unsafeCreate len0 $ \ptr -> do
        N.poke32 (fromIntegral sid)  ptr 0
        N.poke32 (fromErrorCode e) ptr 4
    len = len0 + BS.length debug
    header = FrameHeader len encodeFlags encodeStreamId

buildFramePayloadWindowUpdate :: EncodeInfo -> WindowSize -> (FrameHeader, Builder)
buildFramePayloadWindowUpdate EncodeInfo{..} size = (header, builder)
  where
    -- fixme: reserve bit
    builder = (b4 :)
    b4 = N.bytestring32 $ fromIntegral size
    header = FrameHeader 4 encodeFlags encodeStreamId

buildFramePayloadContinuation :: EncodeInfo -> HeaderBlockFragment -> (FrameHeader, Builder)
buildFramePayloadContinuation EncodeInfo{..} hdr = (header, builder)
  where
    builder = (hdr :)
    len = BS.length hdr
    header = FrameHeader len encodeFlags encodeStreamId

buildFramePayloadUnknown :: EncodeInfo -> ByteString -> (FrameHeader, Builder)
buildFramePayloadUnknown = buildFramePayloadData
