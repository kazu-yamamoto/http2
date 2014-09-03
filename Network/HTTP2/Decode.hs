{-# LANGUAGE TupleSections, BangPatterns, RecordWildCards #-}

module Network.HTTP2.Decode (
    decodeFrame
  ) where

import Control.Applicative ((<$>))
import Control.Monad (void, (>=>))
import qualified Data.Attoparsec.Binary as BI
import qualified Data.Attoparsec.ByteString as B
import Data.Array (Array, listArray, (!))
import Data.Bits (clearBit, shiftL, testBit, (.|.))

import Network.HTTP2.Types

----------------------------------------------------------------

-- | Check the frame header against the settings to ensure that the
-- length of the frame does not exceed our designated frame size
-- (Section 4.2)
checkHeaderLen :: Settings -> FrameHeader -> Maybe ErrorCode
checkHeaderLen settings (FrameHeader len _ _ _)
    | len > maxSize = Just FrameSizeError
    | otherwise     = Nothing
  where
    maxSize = case lookup SettingsMaxFrameSize settings of
        Just x  -> fromIntegral x
        Nothing -> maxFrameSize

-- | Check the various types of frames for basic errors
checkFrameErrors :: Settings -> FrameHeader -> Maybe ErrorCode
checkFrameErrors settings (FrameHeader _len ft _flags sid)
    -- These frames must have a non-zero StreamID
    -- (Sections 6.1, 6.2, 6.3, 6.4, 6.10)
    | ft `elem` nonZeroFrameTypes && sid == streamIdentifierForSeetings   = Just ProtocolError
    -- Settings/Pings/GoAway must use a StreamID of 0 (Section 6.5, 6.7, 6.8)
    | ft `elem` zeroFrameTypes && sid /= streamIdentifierForSeetings      = Just ProtocolError
    -- Push must be enabled for push frames (Section 6.6)
    | ft == FramePushPromise && not pushEnabled = Just ProtocolError
    | otherwise                                 = Nothing
  where
    zeroFrameTypes = [ FrameSettings
                     , FramePing
                     , FrameGoAway
                     ]
    nonZeroFrameTypes = [ FrameData
                        , FrameHeaders
                        , FramePriority
                        , FrameRSTStream
                        , FramePushPromise
                        , FrameContinuation
                        ]
    pushEnabled = case lookup SettingsEnablePush settings of
        Nothing -> True
        Just x  -> x /= 0

----------------------------------------------------------------

-- fixme :: error code
decodeFrame :: B.Parser Frame
decodeFrame = do
    header <- decodeFrameHeader
    Frame header <$> decodeFramePayload header

----------------------------------------------------------------

decodeFrameHeader :: B.Parser FrameHeader
decodeFrameHeader = do
    a <- fromIntegral <$> BI.anyWord16be
    b <- B.anyWord8
    let frameLength = (a `shiftL` 8) .|. fromIntegral b :: FrameLength
    tp <- B.anyWord8
    let mtyp = frameTypeFromWord8 tp
    case mtyp of
        Nothing  -> do
            void $ B.take frameLength
            fail $ "Unknown frame type: " ++ show tp
        Just typ -> do
            flags <- B.anyWord8
            (streamId, _) <- streamIdentifier
            return $ FrameHeader frameLength typ flags streamId

----------------------------------------------------------------

type FramePayloadDecoder = FrameHeader -> B.Parser FramePayload

payloadDecoders :: Array FrameType FramePayloadDecoder
payloadDecoders = listArray (minBound :: FrameType, maxBound :: FrameType)
    [ decodeDataFrame
    , decodeHeadersFrame
    , decodePriorityFrame
    , decodeRstStreamFrame
    , decodeSettingsFrame
    , decodePushPromiseFrame
    , decodePingFrame
    , decodeGoAwayFrame
    , decodeWindowUpdateFrame
    , decodeContinuationFrame
    ]

decodeFramePayload :: FramePayloadDecoder
decodeFramePayload header = decodePayload header
  where
    -- header always contain a valid FrameType
    decodePayload = payloadDecoders ! fhType header

----------------------------------------------------------------

decodeDataFrame :: FramePayloadDecoder
decodeDataFrame header = decodeWithPadding header $ B.take >=> (return . DataFrame)

decodeHeadersFrame :: FramePayloadDecoder
decodeHeadersFrame header = decodeWithPadding header $ \len ->
    if priority then do
        (streamId, excl) <- streamIdentifier
        weight <- (+1) . fromIntegral <$> B.anyWord8
        d <- B.take $ len - 5
        return $ HeaderFrame (Just excl) (Just streamId) (Just weight) d
    else
        HeaderFrame Nothing Nothing Nothing <$> B.take len
  where
    flags = fhFlags header
    priority = testBit flags 6

decodePriorityFrame :: FramePayloadDecoder
decodePriorityFrame _ = do
    (streamId, excl) <- streamIdentifier
    weight <- (+1) . fromIntegral <$> B.anyWord8
    return $ PriorityFrame excl streamId weight

decodeRstStreamFrame :: FramePayloadDecoder
decodeRstStreamFrame _ = RSTStreamFrame . errorCodeFromWord32 <$> BI.anyWord32be

decodeSettingsFrame :: FramePayloadDecoder
decodeSettingsFrame FrameHeader{..}
  -- Goaway: ProtocolError
  | isNotValid = fail "Incorrect frame length"
  | otherwise  = SettingsFrame <$> settings num []
  where
    num = fhLength `div` 6
    isNotValid = fhLength `mod` 6 /= 0
    settings 0 kvs = return kvs
    settings n kvs = do
        rawSetting <- BI.anyWord16be
        let msettings = settingsFromWord16 rawSetting
            n' = n - 1
        case msettings of
            Nothing -> settings n' kvs -- ignoring unknown one (Section 6.5.2)
            Just k  -> do
                v <- BI.anyWord32be
                settings n' ((k,v):kvs)

decodePushPromiseFrame :: FramePayloadDecoder
decodePushPromiseFrame header = decodeWithPadding header $ \len -> do
    (streamId, _) <- streamIdentifier
    hbf <- B.take $ len - 4
    return $ PushPromiseFrame streamId hbf

decodePingFrame :: FramePayloadDecoder
decodePingFrame header
  -- Goaway: FrameSizeError
  | frameLen header /= 8 = fail "Invalid length for ping"
  | otherwise            = PingFrame <$> B.take 8

decodeGoAwayFrame :: FramePayloadDecoder
decodeGoAwayFrame header = do
    (streamId, _) <- streamIdentifier
    errCode <- errorCodeFromWord32 <$> BI.anyWord32be
    debug <- B.take $ frameLen header - 8
    return $ GoAwayFrame streamId errCode debug

decodeWindowUpdateFrame :: FramePayloadDecoder
decodeWindowUpdateFrame header
  -- Goaway: FrameSizeError (not sure)
  | frameLen header /= 4 = fail "Invalid length for window update"
  | otherwise            = do
      (streamId, _) <- streamIdentifier
      return $ WindowUpdateFrame streamId

decodeContinuationFrame :: FramePayloadDecoder
decodeContinuationFrame header = ContinuationFrame <$> B.take (frameLen header)

----------------------------------------------------------------

-- | Helper function to pull off the padding if its there, and will
-- eat up the trailing padding automatically. Calls the parser func
-- passed in with the length of the unpadded portion between the
-- padding octet and the actual padding
decodeWithPadding :: FrameHeader -> (Int -> B.Parser a) -> B.Parser a
decodeWithPadding header p
  | padded = do
      padding <- fromIntegral <$> B.anyWord8
      val <- p $ frameLen header - padding - 1
      void $ B.take padding
      return val
  | otherwise = p $ frameLen header
  where
    flags = fhFlags header
    padded = testBit flags 4

streamIdentifier :: B.Parser (StreamIdentifier, Bool)
streamIdentifier = do
    w32 <- BI.anyWord32be
    let !streamdId = StreamIdentifier $ clearBit w32 31
        !exclusive = testBit w32 31
    return (streamdId, exclusive)

frameLen :: FrameHeader -> Int
frameLen h = fromIntegral $ fhLength h
