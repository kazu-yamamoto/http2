{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2.Frame.Decode (
  -- * Decoding
    decodeFrame
  , decodeFrameHeader
  , checkFrameHeader
  -- * Decoding payload
  , decodeFramePayload
  , FramePayloadDecoder
  , decodeDataFrame
  , decodeHeadersFrame
  , decodePriorityFrame
  , decoderstStreamFrame
  , decodeSettingsFrame
  , decodePushPromiseFrame
  , decodePingFrame
  , decodeGoAwayFrame
  , decodeWindowUpdateFrame
  , decodeContinuationFrame
  ) where

import Data.Array (Array, listArray, (!))
import qualified Data.ByteString as BS
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import qualified Network.ByteOrder as N
import System.IO.Unsafe (unsafeDupablePerformIO)

import Imports
import Network.HTTP2.Frame.Types

----------------------------------------------------------------

-- | Decoding an HTTP/2 frame to 'ByteString'.
-- The second argument must be include the entire of frame.
-- So, this function is not useful for real applications
-- but useful for testing.
decodeFrame :: Settings    -- ^ HTTP/2 settings
            -> ByteString  -- ^ Input byte-stream
            -> Either HTTP2Error Frame -- ^ Decoded frame
decodeFrame settings bs = checkFrameHeader settings (decodeFrameHeader bs0)
                      >>= \(typ,header) -> decodeFramePayload typ header bs1
                      >>= \payload -> return $ Frame header payload
  where
    (bs0,bs1) = BS.splitAt 9 bs

----------------------------------------------------------------

-- | Decoding an HTTP/2 frame header.
--   Must supply 9 bytes.
decodeFrameHeader :: ByteString -> (FrameTypeId, FrameHeader)
decodeFrameHeader = withBS $ \fptr off _ -> unsafeDupablePerformIO $ withForeignPtr fptr $ \ptr -> do
    let p = ptr +. off
    len <- fromIntegral  <$> N.peek24 p 0
    typ <- toFrameTypeId <$> N.peek8  p 3
    flg <-                   N.peek8  p 4
    w32 <-                   N.peek32 p 5
    let sid = streamIdentifier w32
    return (typ, FrameHeader len flg sid)

(+.) :: Ptr Word8 -> Int -> Ptr Word8
(+.) = plusPtr

----------------------------------------------------------------

-- | Checking a frame header and reporting an error if any.
--
-- >>> checkFrameHeader defaultSettings (FrameData,(FrameHeader 100 0 0))
-- Left (ConnectionError ProtocolError "cannot used in control stream")
checkFrameHeader :: Settings
                 -> (FrameTypeId, FrameHeader)
                 -> Either HTTP2Error (FrameTypeId, FrameHeader)
checkFrameHeader Settings {..} typfrm@(typ,FrameHeader {..})
  | payloadLength > maxFrameSize =
      Left $ ConnectionError FrameSizeError "exceeds maximum frame size"
  | typ `elem` nonZeroFrameTypes && isControl streamId =
      Left $ ConnectionError ProtocolError "cannot used in control stream"
  | typ `elem` zeroFrameTypes && not (isControl streamId) =
      Left $ ConnectionError ProtocolError "cannot used in non-zero stream"
  | otherwise = checkType typ
  where
    checkType FrameHeaders
      | testPadded flags && payloadLength < 1 =
        Left $ ConnectionError FrameSizeError "insufficient payload for Pad Length"
      | testPriority flags && payloadLength < 5 =
        Left $ ConnectionError FrameSizeError "insufficient payload for priority fields"
      | testPadded flags && testPriority flags && payloadLength < 6 =
        Left $ ConnectionError FrameSizeError "insufficient payload for Pad Length and priority fields"
    checkType FramePriority | payloadLength /= 5 =
        Left $ StreamError FrameSizeError streamId
    checkType FrameRSTStream | payloadLength /= 4 =
        Left $ ConnectionError FrameSizeError "payload length is not 4 in rst stream frame"
    checkType FrameSettings
      | payloadLength `mod` 6 /= 0 =
        Left $ ConnectionError FrameSizeError "payload length is not multiple of 6 in settings frame"
      | testAck flags && payloadLength /= 0 =
        Left $ ConnectionError FrameSizeError "payload length must be 0 if ack flag is set"
    checkType FramePushPromise
      | not enablePush =
        Left $ ConnectionError ProtocolError "push not enabled" -- checkme
      | isClientInitiated streamId =
        Left $ ConnectionError ProtocolError "push promise must be used with even stream identifier"
    checkType FramePing | payloadLength /= 8 =
        Left $ ConnectionError FrameSizeError "payload length is 8 in ping frame"
    checkType FrameGoAway | payloadLength < 8 =
        Left $ ConnectionError FrameSizeError "goaway body must be 8 bytes or larger"
    checkType FrameWindowUpdate | payloadLength /= 4 =
        Left $ ConnectionError FrameSizeError "payload length is 4 in window update frame"
    checkType _ = Right typfrm

zeroFrameTypes :: [FrameTypeId]
zeroFrameTypes = [
    FrameSettings
  , FramePing
  , FrameGoAway
  ]

nonZeroFrameTypes :: [FrameTypeId]
nonZeroFrameTypes = [
    FrameData
  , FrameHeaders
  , FramePriority
  , FrameRSTStream
  , FramePushPromise
  , FrameContinuation
  ]

----------------------------------------------------------------

-- | The type for frame payload decoder.
type FramePayloadDecoder = FrameHeader -> ByteString
                        -> Either HTTP2Error FramePayload

payloadDecoders :: Array Word8 FramePayloadDecoder
payloadDecoders = listArray (minFrameType, maxFrameType)
    [ decodeDataFrame
    , decodeHeadersFrame
    , decodePriorityFrame
    , decoderstStreamFrame
    , decodeSettingsFrame
    , decodePushPromiseFrame
    , decodePingFrame
    , decodeGoAwayFrame
    , decodeWindowUpdateFrame
    , decodeContinuationFrame
    ]

-- | Decoding an HTTP/2 frame payload.
--   This function is considered to return a frame payload decoder
--   according to a frame type.
decodeFramePayload :: FrameTypeId -> FramePayloadDecoder
decodeFramePayload (FrameUnknown typ) = checkFrameSize $ decodeUnknownFrame typ
decodeFramePayload ftyp               = checkFrameSize decoder
  where
    decoder = payloadDecoders ! fromFrameTypeId ftyp

----------------------------------------------------------------

-- | Frame payload decoder for DATA frame.
decodeDataFrame :: FramePayloadDecoder
decodeDataFrame header bs = decodeWithPadding header bs DataFrame

-- | Frame payload decoder for HEADERS frame.
decodeHeadersFrame :: FramePayloadDecoder
decodeHeadersFrame header bs = decodeWithPadding header bs $ \bs' ->
    if hasPriority then
        let (bs0,bs1) = BS.splitAt 5 bs'
            p = priority bs0
        in HeadersFrame (Just p) bs1
    else
        HeadersFrame Nothing bs'
  where
    hasPriority = testPriority $ flags header

-- | Frame payload decoder for PRIORITY frame.
decodePriorityFrame :: FramePayloadDecoder
decodePriorityFrame _ bs = Right $ PriorityFrame $ priority bs

-- | Frame payload decoder for RST_STREAM frame.
decoderstStreamFrame :: FramePayloadDecoder
decoderstStreamFrame _ bs = Right $ RSTStreamFrame $ toErrorCodeId (N.word32 bs)

-- | Frame payload decoder for SETTINGS frame.
decodeSettingsFrame :: FramePayloadDecoder
decodeSettingsFrame FrameHeader{..}
  | num > 10  = const $ Left $ ConnectionError EnhanceYourCalm "Settings is too large"
  | otherwise = withBS $ \fptr off _ -> Right $ SettingsFrame $ unsafeDupablePerformIO $ withForeignPtr fptr $ \ptr -> do
        let p = ptr +. off
        settings num p id
  where
    num = payloadLength `div` 6
    settings 0 _ builder = return $ builder []
    settings n p builder = do
        rawSetting <- N.peek16 p 0
        let msettings = toSettingsKeyId rawSetting
            n' = n - 1
        case msettings of
            Nothing -> settings n' (p +. 6) builder -- ignoring unknown one (Section 6.5.2)
            Just k  -> do
                w32 <- N.peek32 p 2
                let v = fromIntegral w32
                settings n' (p +. 6) (builder. ((k,v):))

-- | Frame payload decoder for PUSH_PROMISE frame.
decodePushPromiseFrame :: FramePayloadDecoder
decodePushPromiseFrame header bs = decodeWithPadding header bs $ \bs' ->
    let (bs0,bs1) = BS.splitAt 4 bs'
        sid = streamIdentifier (N.word32 bs0)
    in PushPromiseFrame sid bs1

-- | Frame payload decoder for PING frame.
decodePingFrame :: FramePayloadDecoder
decodePingFrame _ bs = Right $ PingFrame bs

-- | Frame payload decoder for GOAWAY frame.
decodeGoAwayFrame :: FramePayloadDecoder
decodeGoAwayFrame _ bs = Right $ GoAwayFrame sid ecid bs2
  where
    (bs0,bs1') = BS.splitAt 4 bs
    (bs1,bs2)  = BS.splitAt 4 bs1'
    sid = streamIdentifier (N.word32 bs0)
    ecid = toErrorCodeId (N.word32 bs1)

-- | Frame payload decoder for WINDOW_UPDATE frame.
decodeWindowUpdateFrame :: FramePayloadDecoder
decodeWindowUpdateFrame _ bs
  | wsi == 0  = Left $ ConnectionError ProtocolError "window update must not be 0"
  | otherwise = Right $ WindowUpdateFrame wsi
  where
    wsi = fromIntegral (N.word32 bs `clearBit` 31)

-- | Frame payload decoder for CONTINUATION frame.
decodeContinuationFrame :: FramePayloadDecoder
decodeContinuationFrame _ bs = Right $ ContinuationFrame bs

decodeUnknownFrame :: FrameType -> FramePayloadDecoder
decodeUnknownFrame typ _ bs = Right $ UnknownFrame typ bs

----------------------------------------------------------------

checkFrameSize :: FramePayloadDecoder -> FramePayloadDecoder
checkFrameSize func header@FrameHeader{..} body
  | payloadLength > BS.length body =
      Left $ ConnectionError FrameSizeError "payload is too short"
  | otherwise = func header body

-- | Helper function to pull off the padding if its there, and will
-- eat up the trailing padding automatically. Calls the decoder func
-- passed in with the length of the unpadded portion between the
-- padding octet and the actual padding
decodeWithPadding :: FrameHeader -> ByteString -> (ByteString -> FramePayload) -> Either HTTP2Error FramePayload
decodeWithPadding FrameHeader{..} bs body
  | padded = let Just (w8,rest) = BS.uncons bs
                 padlen = intFromWord8 w8
                 bodylen = payloadLength - padlen - 1
             in if bodylen < 0 then
                    Left $ ConnectionError ProtocolError "padding is not enough"
                  else
                    Right . body $ BS.take bodylen rest
  | otherwise = Right $ body bs
  where
    padded = testPadded flags

streamIdentifier :: Word32 -> StreamId
streamIdentifier w32 = clearExclusive $ fromIntegral w32

priority :: ByteString -> Priority
priority = withBS $ \fptr off _ -> unsafeDupablePerformIO $ withForeignPtr fptr $ \ptr -> do
    let p = ptr +. off
    w32 <- N.peek32 p 0
    let streamdId = streamIdentifier w32
        exclusive = testExclusive (fromIntegral w32) -- fixme
    w8 <- N.peek8 p 4
    let weight = intFromWord8 w8 + 1
    return $ Priority exclusive streamdId weight

intFromWord8 :: Word8 -> Int
intFromWord8 = fromIntegral

----------------------------------------------------------------
-- Compatibility helpers for bytestring

withBS :: (ForeignPtr Word8 -> Int -> Int -> a) -> ByteString -> a
#if MIN_VERSION_bytestring(0,11,0)
withBS f (BS fptr len) = f fptr 0 len
#else
withBS f (PS fptr off len) = f fptr off len
#endif
