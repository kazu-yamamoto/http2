{-# LANGUAGE TupleSections, BangPatterns, RecordWildCards, OverloadedStrings #-}

module Network.HTTP2.Decode (
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

import Control.Applicative ((<$>))
import Data.Array (Array, listArray, (!))
import Data.Bits (clearBit, shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString(..), inlinePerformIO)
import Data.Word
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek)

import Network.HTTP2.Types

----------------------------------------------------------------

-- | Decoding an HTTP/2 frame to 'ByteString'.
-- The second argument must be include the entire of frame.
-- So, this function is not useful for real applications
-- but useful for testing.
decodeFrame :: Settings    -- ^ HTTP/2 settings
            -> ByteString  -- ^ Input byte-stream
            -> Either HTTP2Error Frame -- ^ Decoded frame
decodeFrame settings bs = case mftype of
    (FrameUnknown _) -> Right Frame { frameHeader = header
                                    , framePayload = decodeUnknownFrame typ bs1
                                    }
    ftyp -> case checkFrameHeader settings ftyp header of
        Just h2err -> Left h2err
        Nothing    -> Right Frame { frameHeader = header
                                  , framePayload = decodeFramePayload typ header bs1
                                  }
  where
    (bs0,bs1) = BS.splitAt 9 bs
    (typ, header) = decodeFrameHeader bs0
    mftype = toFrameTypeId typ

----------------------------------------------------------------

-- | Must supply 9 bytes.
decodeFrameHeader :: ByteString -> (FrameType, FrameHeader)
decodeFrameHeader (PS fptr off _) = inlinePerformIO $ withForeignPtr fptr $ \ptr -> do
    let p = ptr +. off
    l0 <- fromIntegral <$> peek p
    l1 <- fromIntegral <$> peek (p +. 1)
    l2 <- fromIntegral <$> peek (p +. 2)
    typ <-                 peek (p +. 3)
    flg <-                 peek (p +. 4)
    w32 <- word32' (p +. 5)
    let !len = (l0 `shiftL` 16) .|. (l1 `shiftL` 8) .|. l2
        !sid = streamIdentifier w32
    return $ (typ, FrameHeader len flg sid)

(+.) :: Ptr Word8 -> Int -> Ptr Word8
(+.) = plusPtr

----------------------------------------------------------------

-- | Checking a frame header and reporting an error if any.
--
-- >>> let stid = toStreamIdentifier 0
-- >>> checkFrameHeader defaultSettings FrameData (FrameHeader 100 0 stid)
-- Just (ConnectionError ProtocolError "cannot used in control stream")
checkFrameHeader :: Settings -> FrameTypeId -> FrameHeader -> Maybe HTTP2Error
checkFrameHeader Settings {..} typ FrameHeader {..}
  | payloadLength > maxFrameSize =
      Just $ ConnectionError FrameSizeError "exceeds maximum frame size"
  | typ `elem` nonZeroFrameTypes && isControl streamId =
      Just $ ConnectionError ProtocolError "cannot used in control stream"
  | typ `elem` zeroFrameTypes && not (isControl streamId) =
      Just $ ConnectionError ProtocolError "cannot used in non-zero stream"
  | otherwise = checkType typ
  where
    checkType FramePriority | payloadLength /= 5 =
        Just $ StreamError FrameSizeError streamId
    checkType FrameRSTStream | payloadLength /= 4 =
        Just $ ConnectionError FrameSizeError "payload length is not 4 in rst stream frame"
    checkType FrameSettings | payloadLength `mod` 6 /= 0 =
        Just $ ConnectionError FrameSizeError "payload length is not multiple of 6 in settings frame"
    checkType FramePushPromise | not enablePush =
        Just $ ConnectionError ProtocolError "push not enabled" -- checkme
    checkType FramePing | payloadLength /= 8 =
        Just $ ConnectionError FrameSizeError "payload length is 8 in ping frame"
    checkType FrameWindowUpdate | payloadLength /= 4 =
        Just $ ConnectionError FrameSizeError "payload length is 4 in window update frame"
    checkType _ = Nothing

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

type FramePayloadDecoder = FrameHeader -> ByteString -> FramePayload

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

decodeFramePayload :: FrameType -> FramePayloadDecoder
decodeFramePayload ftyp = payloadDecoders ! ftyp

----------------------------------------------------------------

decodeDataFrame :: FramePayloadDecoder
decodeDataFrame header bs = decodeWithPadding header bs DataFrame

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

decodePriorityFrame :: FramePayloadDecoder
decodePriorityFrame _ bs = PriorityFrame $ priority bs

decoderstStreamFrame :: FramePayloadDecoder
decoderstStreamFrame _ bs = RSTStreamFrame $ toErrorCodeId (word32 bs)

decodeSettingsFrame :: FramePayloadDecoder
decodeSettingsFrame FrameHeader{..} (PS fptr off _) = SettingsFrame alist
  where
    num = payloadLength `div` 6
    alist = inlinePerformIO $ withForeignPtr fptr $ \ptr -> do
        let p = ptr +. off
        settings num p id
    settings 0 _ builder = return $ builder []
    settings n p builder = do
        rawSetting <- word16' p
        let msettings = toSettingsKeyId rawSetting
            n' = n - 1
        case msettings of
            Nothing -> settings n' (p +. 6) builder -- ignoring unknown one (Section 6.5.2)
            Just k  -> do
                w32 <- word32' (p +. 2)
                let v = fromIntegral w32
                settings n' (p +. 6) (builder. ((k,v):))

decodePushPromiseFrame :: FramePayloadDecoder
decodePushPromiseFrame header bs = decodeWithPadding header bs $ \bs' ->
    let (bs0,bs1) = BS.splitAt 4 bs'
        sid = streamIdentifier (word32 bs0)
    in PushPromiseFrame sid bs1

decodePingFrame :: FramePayloadDecoder
decodePingFrame _ bs = PingFrame bs

decodeGoAwayFrame :: FramePayloadDecoder
decodeGoAwayFrame _ bs = GoAwayFrame sid ecid bs2
  where
    (bs0,bs1') = BS.splitAt 4 bs
    (bs1,bs2)  = BS.splitAt 4 bs1'
    sid = streamIdentifier (word32 bs0)
    ecid = toErrorCodeId (word32 bs1)

decodeWindowUpdateFrame :: FramePayloadDecoder
decodeWindowUpdateFrame _ bs = WindowUpdateFrame wsi
  where
    !wsi = word32 bs `clearBit` 31

decodeContinuationFrame :: FramePayloadDecoder
decodeContinuationFrame _ bs = ContinuationFrame bs

decodeUnknownFrame :: FrameType -> ByteString -> FramePayload
decodeUnknownFrame typ bs = UnknownFrame typ bs

----------------------------------------------------------------

-- | Helper function to pull off the padding if its there, and will
-- eat up the trailing padding automatically. Calls the decoder func
-- passed in with the length of the unpadded portion between the
-- padding octet and the actual padding
decodeWithPadding :: FrameHeader -> ByteString -> (ByteString -> FramePayload) -> FramePayload
decodeWithPadding FrameHeader{..} bs body
  | padded = let Just (w8,rest) = BS.uncons bs
                 padlen = intFromWord8 w8
                 rest' = BS.take (payloadLength - padlen - 1) rest
             in body rest'
  | otherwise = body bs
  where
    padded = testPadded flags

streamIdentifier :: Word32 -> StreamIdentifier
streamIdentifier w32 = toStreamIdentifier (fromIntegral w32)

priority :: ByteString -> Priority
priority (PS fptr off _) = inlinePerformIO $ withForeignPtr fptr $ \ptr -> do
    let p = ptr +. off
    w32 <- word32' p
    let !streamdId = streamIdentifier w32
        !exclusive = testExclusive (fromIntegral w32) -- fixme
    w8 <- peek (p +. 4)
    let weight = intFromWord8 w8 + 1
    return $ Priority exclusive streamdId weight

intFromWord8 :: Word8 -> Int
intFromWord8 = fromIntegral

word32 :: ByteString -> Word32
word32 (PS fptr off _) = inlinePerformIO $ withForeignPtr fptr $ \ptr -> do
    let p = ptr +. off
    word32' p
{-# INLINE word32 #-}

word32' :: Ptr Word8 -> IO Word32
word32' p = do
    w0 <- fromIntegral <$> peek p
    w1 <- fromIntegral <$> peek (p +. 1)
    w2 <- fromIntegral <$> peek (p +. 2)
    w3 <- fromIntegral <$> peek (p +. 3)
    let !w32 = (w0 `shiftL` 24) .|. (w1 `shiftL` 16) .|. (w2 `shiftL` 8) .|. w3
    return w32
{-# INLINE word32' #-}

word16' :: Ptr Word8 -> IO Word16
word16' p = do
    w0 <- fromIntegral <$> peek p
    w1 <- fromIntegral <$> peek (p +. 1)
    let !w16 = (w0 `shiftL` 8) .|. w1
    return w16
{-# INLINE word16' #-}
