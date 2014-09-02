{-# LANGUAGE TupleSections, BangPatterns #-}

module Network.HTTP2.Decode where

import Control.Applicative ((<$>))
import Control.Monad (replicateM, void, (>=>))
import qualified Data.Attoparsec.Binary as BI
import qualified Data.Attoparsec.ByteString as B
import Data.Bits (clearBit, shiftL, testBit, (.|.))
import qualified Data.Map as Map

import Network.HTTP2.Types

-- Our basic FrameParser type
type FrameParser = FrameHeader -> B.Parser Frame

-- A full frame of the header with the frame contents
type FullFrame = (FrameHeader, Frame)

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

-- fixme
deocdeMap :: Map.Map FrameType FrameParser
deocdeMap = Map.fromList
    [ (FrameData, deocdeDataFrame)
    , (FrameHeaders, deocdeHeadersFrame)
    , (FramePriority, deocdePriorityFrame)
    , (FrameRSTStream, deocdeRstStreamFrame)
    , (FrameSettings, deocdeSettingsFrame)
    , (FramePushPromise, deocdePushPromiseFrame)
    , (FramePing, deocdePingFrame)
    , (FrameGoAway, deocdeGoAwayFrame)
    , (FrameWindowUpdate, deocdeWindowUpdateFrame)
    , (FrameContinuation, deocdeContinuationFrame)
    ]

-- fixme
deocdeFrameBody :: RawFrame -> Either String FullFrame
deocdeFrameBody (RawFrame header body) = do
    fp <- case Map.lookup (fhType header) deocdeMap of
        Nothing -> fail "Unable to locate parser for frame type"
        Just fp -> return fp
    frameBody <- B.parseOnly (fp header) body
    return (header, frameBody)

deocdeFrameHeader :: B.Parser FrameHeader
deocdeFrameHeader = do
    a <- fromIntegral <$> BI.anyWord16be
    b <- B.anyWord8
    let frameLength = (a `shiftL` 8) .|. fromIntegral b :: FrameLength
    tp <- B.anyWord8
    let mtyp = frameTypeFromWord8 tp
    case mtyp of
        Nothing  -> fail $ "Unknown frame type: " ++ show tp
        Just typ -> do
            flags <- B.anyWord8
            (streamId, _) <- steramIdentifier
            return $ FrameHeader frameLength typ flags streamId

deocdeRawFrame :: B.Parser RawFrame
deocdeRawFrame = do
    header <- deocdeFrameHeader
    payload <- B.take $ fromIntegral $ fhLength header
    return $ RawFrame header payload

-- fixme
deocdeUnknownFrame :: FrameParser
deocdeUnknownFrame header = UnknownFrame <$> B.take (frameLen header)

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

deocdeDataFrame :: FrameParser
deocdeDataFrame header = paddingParser header $ B.take >=> (return . DataFrame)

deocdeHeadersFrame :: FrameParser
deocdeHeadersFrame header = paddingParser header $ \len ->
    if priority then do
        (streamId, excl) <- steramIdentifier
        weight <- (+1) . fromIntegral <$> B.anyWord8
        d <- B.take $ len - 5
        return $ HeaderFrame (Just excl) (Just streamId) (Just weight) d
    else
        HeaderFrame Nothing Nothing Nothing <$> B.take len
  where
    flags = fhFlags header
    priority = testBit flags 6

deocdePriorityFrame :: FrameParser
deocdePriorityFrame _ = do
    (streamId, excl) <- steramIdentifier
    weight <- (+1) . fromIntegral <$> B.anyWord8
    return $ PriorityFrame excl streamId weight

deocdeRstStreamFrame :: FrameParser
deocdeRstStreamFrame _ = do
    w32 <- BI.anyWord32be
    let merr = errorCodeFromWord32 w32
    case merr of
        Nothing  -> fail $ "Unknown error code in RST_STREAM" ++ show w32
        Just err -> return $ RSTStreamFrame err

deocdeSettingsFrame :: FrameParser
deocdeSettingsFrame header
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

deocdePushPromiseFrame :: FrameParser
deocdePushPromiseFrame header = paddingParser header $ \len -> do
    (streamId, _) <- steramIdentifier
    hbf <- B.take $ len - 4
    return $ PushPromiseFrame streamId hbf

deocdePingFrame :: FrameParser
deocdePingFrame header =
    if frameLen header /= 8 then
        fail "Invalid length for ping"
    else
        PingFrame <$> B.take 8

deocdeGoAwayFrame :: FrameParser
deocdeGoAwayFrame header = do
    (streamId, _) <- steramIdentifier
    ec <- BI.anyWord32be
    let merrCode = errorCodeFromWord32 ec
    debug <- B.take $ frameLen header - 8
    case merrCode of
        Nothing      -> fail $ "Unknown error code: " ++ show ec
        Just errCode -> return $ GoAwayFrame streamId errCode debug

deocdeWindowUpdateFrame :: FrameParser
deocdeWindowUpdateFrame header
  | frameLen header /= 4 = fail "Invalid length for window update"
  | otherwise            = do
      (streamId, _) <- steramIdentifier
      return $ WindowUpdateFrame streamId

deocdeContinuationFrame :: FrameParser
deocdeContinuationFrame header = ContinuationFrame <$> B.take (frameLen header)

steramIdentifier :: B.Parser (StreamIdentifier, Bool)
steramIdentifier = do
    w32 <- BI.anyWord32be
    let !streamdId = StreamIdentifier $ clearBit w32 31
        !exclusive = testBit w32 31
    return (streamdId, exclusive)
