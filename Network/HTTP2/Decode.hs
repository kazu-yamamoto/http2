{-# LANGUAGE TupleSections, BangPatterns #-}

module Network.HTTP2.Decode where

import Control.Applicative ((<$>))
import Control.Monad (replicateM, void, (>=>))
import qualified Data.Attoparsec.Binary as BI
import qualified Data.Attoparsec.ByteString as B
import Data.Bits (clearBit, shiftL, testBit, (.|.))
import qualified Data.Map as Map

import Network.HTTP2.Types

----------------------------------------------------------------

frameLen :: FrameHeader -> Int
frameLen h = fromIntegral $ fhLength h

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
            -- fixme: consume
            fail $ "Unknown frame type: " ++ show tp
        Just typ -> do
            flags <- B.anyWord8
            (streamId, _) <- streamIdentifier
            return $ FrameHeader frameLength typ flags streamId

----------------------------------------------------------------

type FramePayloadDecoder = FrameHeader -> B.Parser FramePayload

-- fixme :: Array
decodeMap :: Map.Map FrameType FramePayloadDecoder
decodeMap = Map.fromList
    [ (FrameData, decodeDataFrame)
    , (FrameHeaders, decodeHeadersFrame)
    , (FramePriority, decodePriorityFrame)
    , (FrameRSTStream, decodeRstStreamFrame)
    , (FrameSettings, decodeSettingsFrame)
    , (FramePushPromise, decodePushPromiseFrame)
    , (FramePing, decodePingFrame)
    , (FrameGoAway, decodeGoAwayFrame)
    , (FrameWindowUpdate, decodeWindowUpdateFrame)
    , (FrameContinuation, decodeContinuationFrame)
    ]

decodeFramePayload :: FramePayloadDecoder
decodeFramePayload header = do
    decodePayload <- case Map.lookup (fhType header) decodeMap of
        Nothing -> do
            -- fixme: consume
            fail "Unable to locate parser for frame type"
        Just fp -> return fp
    decodePayload header

----------------------------------------------------------------

-- fixme
decodeUnknownFrame :: FramePayloadDecoder
decodeUnknownFrame header = UnknownFrame <$> B.take (frameLen header)

-- | Helper function to pull off the padding if its there, and will
-- eat up the trailing padding automatically. Calls the parser func
-- passed in with the length of the unpadded portion between the
-- padding octet and the actual padding
paddingParser :: FrameHeader -> (Int -> B.Parser a) -> B.Parser a
paddingParser header p =
    if padded then do
        padding <- fromIntegral <$> B.anyWord8
        val <- p $ frameLen header - padding - 1
        void $ B.take padding
        return val
    else p (frameLen header)
  where
    flags = fhFlags header
    padded = testBit flags 4

decodeDataFrame :: FramePayloadDecoder
decodeDataFrame header = paddingParser header $ B.take >=> (return . DataFrame)

decodeHeadersFrame :: FramePayloadDecoder
decodeHeadersFrame header = paddingParser header $ \len ->
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
decodeRstStreamFrame _ = do
    w32 <- BI.anyWord32be
    let merr = errorCodeFromWord32 w32
    case merr of
        Nothing  -> fail $ "Unknown error code in RST_STREAM" ++ show w32
        Just err -> return $ RSTStreamFrame err

decodeSettingsFrame :: FramePayloadDecoder
decodeSettingsFrame header
  | isNotValid = fail "Incorrect frame length"
  | otherwise  = SettingsFrame <$> settings
  where
    frameLength = fromIntegral $ fhLength header
    n = frameLength `div` 6
    isNotValid = frameLength `mod` 6 /= 0
    settings = replicateM n $ do
        rawSetting <- BI.anyWord16be
        let msettings = settingsFromWord16 rawSetting
        case msettings of
            Nothing -> fail $ "Unknown settings: " ++ show rawSetting
            Just s  -> (s,) <$> BI.anyWord32be

decodePushPromiseFrame :: FramePayloadDecoder
decodePushPromiseFrame header = paddingParser header $ \len -> do
    (streamId, _) <- streamIdentifier
    hbf <- B.take $ len - 4
    return $ PushPromiseFrame streamId hbf

decodePingFrame :: FramePayloadDecoder
decodePingFrame header =
    if frameLen header /= 8 then
        fail "Invalid length for ping"
    else
        PingFrame <$> B.take 8

decodeGoAwayFrame :: FramePayloadDecoder
decodeGoAwayFrame header = do
    (streamId, _) <- streamIdentifier
    ec <- BI.anyWord32be
    let merrCode = errorCodeFromWord32 ec
    debug <- B.take $ frameLen header - 8
    case merrCode of
        Nothing      -> fail $ "Unknown error code: " ++ show ec
        Just errCode -> return $ GoAwayFrame streamId errCode debug

decodeWindowUpdateFrame :: FramePayloadDecoder
decodeWindowUpdateFrame header
  | frameLen header /= 4 = fail "Invalid length for window update"
  | otherwise            = do
      (streamId, _) <- streamIdentifier
      return $ WindowUpdateFrame streamId

decodeContinuationFrame :: FramePayloadDecoder
decodeContinuationFrame header = ContinuationFrame <$> B.take (frameLen header)

streamIdentifier :: B.Parser (StreamIdentifier, Bool)
streamIdentifier = do
    w32 <- BI.anyWord32be
    let !streamdId = StreamIdentifier $ clearBit w32 31
        !exclusive = testBit w32 31
    return (streamdId, exclusive)
