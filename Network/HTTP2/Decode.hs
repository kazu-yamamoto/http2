{-# LANGUAGE TupleSections #-}

module Network.HTTP2.Decode where

import           Control.Applicative        ((<$>))
import           Control.Monad              (replicateM, void, when, (>=>))
import qualified Data.Attoparsec.Binary     as BI
import qualified Data.Attoparsec.ByteString as B
import           Data.Bits                  (clearBit, shiftL, testBit, (.|.))
import qualified Data.Map                   as Map

import Network.HTTP2.Types

-- Our basic FrameParser type
type FrameParser = FrameHeader -> B.Parser Frame

-- A full frame of the header with the frame contents
type FullFrame = (FrameHeader, Frame)

frameLen :: FrameHeader -> Int
frameLen h = fromIntegral $ fhLength h

-- | Check the frame header against the settings to ensure that the length of the
-- frame does not exceed our designated frame size (Section 4.2)
checkHeaderLen :: SettingsMap -> FrameHeader -> Maybe ErrorCode
checkHeaderLen settings (FrameHeader _ _ len _)
    | len > maxFrameSize = Just FrameSizeError
    | otherwise          = Nothing
  where
    -- fixme
    minSize = round $ 2 ** 14
    maxFrameSize =
        fromIntegral $ Map.findWithDefault minSize SettingsMaxFrameSize settings

-- | Check the various types of frames for basic errors
checkFrameErrors :: SettingsMap -> FrameHeader -> Maybe ErrorCode
checkFrameErrors settings (FrameHeader ft _flags _len sid)
    -- These frames must have a non-zero StreamID
    -- (Sections 6.1, 6.2, 6.3, 6.4, 6.10)
    | ft `elem` nonZeroFrameTypes && sid == 0   = Just ProtocolError
    -- Settings/Pings/GoAway must use a StreamID of 0 (Section 6.5, 6.7, 6.8)
    | ft `elem` zeroFrameTypes && sid /= 0      = Just ProtocolError
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
    pushEnabled = Map.findWithDefault 1 SettingsEnablePush settings /= 0

parseMap :: Map.Map FrameType FrameParser
parseMap = Map.fromList
    [ (FrameData, parseDataFrame)
    , (FrameHeaders, parseHeadersFrame)
    , (FramePriority, parsePriorityFrame)
    , (FrameRSTStream, parseRstStreamFrame)
    , (FrameSettings, parseSettingsFrame)
    , (FramePushPromise, parsePushPromiseFrame)
    , (FramePing, parsePingFrame)
    , (FrameGoAway, parseGoAwayFrame)
    , (FrameWindowUpdate, parseWindowUpdateFrame)
    , (FrameContinuation, parseContinuationFrame)
    ]

parseFrameBody :: RawFrame -> Either String FullFrame
parseFrameBody (RawFrame header body) = do
    fp <- case Map.lookup (fhType header) parseMap of
        Nothing -> fail "Unable to locate parser for frame type"
        Just fp -> return fp
    frameBody <- B.parseOnly (fp header) body
    return (header, frameBody)

parseFrameHeader :: B.Parser FrameHeader
parseFrameHeader = do
    a <- fromIntegral <$> BI.anyWord16be
    b <- B.anyWord8
    let frameLength = (a `shiftL` 8) .|. fromIntegral b :: Int24
    tp <- B.anyWord8
    let mtyp = frameTypeFromWord8 tp
    case mtyp of
        Nothing  -> fail $ "Unknown frame type: " ++ show tp
        Just typ -> do
            flags <- B.anyWord8
            streamId <- (`clearBit` 31) <$> BI.anyWord32be
            return $ FrameHeader typ flags frameLength streamId

parseRawFrame :: B.Parser RawFrame
parseRawFrame = do
    header <- parseFrameHeader
    payload <- B.take $ fromIntegral $ fhLength header
    return $ RawFrame header payload

-- fixme
parseUnknownFrame :: FrameParser
parseUnknownFrame header = UnknownFrame <$> B.take (frameLen header)

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

parseDataFrame :: FrameParser
parseDataFrame header = paddingParser header $ B.take >=> (return . DataFrame)

parseHeadersFrame :: FrameParser
parseHeadersFrame header = paddingParser header $ \len ->
    if priority then do
        eAndStream <- BI.anyWord32be
        weight <- Just . (+1) . fromIntegral <$> B.anyWord8
        let excl   = Just $ testBit eAndStream 31
            stream = Just . fromIntegral $ clearBit eAndStream 31
        d <- B.take $ len - 5
        return $ HeaderFrame excl stream weight d
    else
        HeaderFrame Nothing Nothing Nothing <$> B.take len
  where
    flags = fhFlags header
    priority = testBit flags 6

parsePriorityFrame :: FrameParser
parsePriorityFrame _ = do
    eAndStream <- BI.anyWord32be
    weight <- (+1) . fromIntegral <$> B.anyWord8
    let excl = testBit eAndStream 31
        stream = fromIntegral $ clearBit eAndStream 31
    return $ PriorityFrame excl stream weight

parseRstStreamFrame :: FrameParser
parseRstStreamFrame _ =  RSTStreamFrame <$> BI.anyWord32be

parseSettingsFrame :: FrameParser
parseSettingsFrame header = do
    when (frameLength `mod` 6 /= 0) $ fail "Incorrect frame length"
    settings <- replicateM (frameLength `div` 6) $ do
        rawSetting <- BI.anyWord16be
        let msettings = settingsFromWord16 rawSetting
        case msettings of
            Nothing -> fail $ "Invalid settings: " ++ show rawSetting
            Just s  -> (s,) <$> BI.anyWord32be
    return $ SettingsFrame (Map.fromList settings)
  where
    frameLength = fromIntegral $ fhLength header

parsePushPromiseFrame :: FrameParser
parsePushPromiseFrame header = paddingParser header $ \len -> do
    rAndStreamId <- BI.anyWord32be
    let streamId = fromIntegral $ clearBit rAndStreamId 31
    hbf <- B.take $ len - 4
    return $ PushPromiseFrame streamId hbf

parsePingFrame :: FrameParser
parsePingFrame header =
    if frameLen header /= 8 then
        fail "Invalid length for ping"
    else
        PingFrame <$> B.take 8

parseGoAwayFrame :: FrameParser
parseGoAwayFrame header = do
    rAndLastStreamId <- BI.anyWord32be
    ec <- BI.anyWord32be
    let merrCode = errorCodeFromWord32 ec
    debug <- B.take $ frameLen header - 8
    let streamId = fromIntegral $ clearBit rAndLastStreamId 31
    case merrCode of
        Nothing      -> fail $ "Unknown error code: " ++ show ec
        Just errCode -> return $ GoAwayFrame streamId errCode debug

parseWindowUpdateFrame :: FrameParser
parseWindowUpdateFrame header =
    if frameLen header /= 4 then
        fail "Invalid length for window update"
    else
        WindowUpdateFrame <$> (fromIntegral . (`clearBit` 31) <$> BI.anyWord32be)

parseContinuationFrame :: FrameParser
parseContinuationFrame header = ContinuationFrame <$> B.take (frameLen header)
