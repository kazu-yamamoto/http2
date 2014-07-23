{-# LANGUAGE TupleSections #-}

module Network.HTTP2.Frames
    (
    ) where

import           Control.Applicative        ((<$>))
import           Control.Monad              (replicateM, void, when)
import qualified Data.Attoparsec.Binary     as BI
import qualified Data.Attoparsec.ByteString as B
import           Data.Bits                  (clearBit, shiftL, testBit, (.|.))
import           Data.ByteString            (ByteString)
import           Data.Int                   (Int32)
import qualified Data.Map                   as Map
import           Data.Word                  (Word16, Word32, Word8)

import Network.HTTP2.Errors (ErrorCode, errorCodeFromWord32)

type Int24 = Int32
type Int31 = Int32
type RSTStreamErrorCode = Word32
type HeaderBlockFragment = ByteString
type StreamDependency = Int31
type LastStreamId = Int31
type PromisedStreamId = Int31
type Exclusive = Bool
type Weight = Int

type FrameParser = FrameHeader -> B.Parser Frame

type SettingsMap = Map.Map SettingID Word32

data SettingID = SettingHeaderTableSize
               | SettingEnablePush
               | SettingMaxConcurrentStreams
               | SettingInitialWindowSize
               | SettingUnknown
               deriving (Show, Eq, Ord, Enum, Bounded)

data FrameType = FrameData
               | FrameHeaders
               | FramePriority
               | FrameRSTStream
               | FrameSettings
               | FramePushPromise
               | FramePing
               | FrameGoAway
               | FrameWindowUpdate
               | FrameContinuation
               | FrameUnknown
               deriving (Show, Eq, Ord, Enum, Bounded)

data FrameHeader = FrameHeader
    { fhType     :: FrameType
    , fhFlags    :: Word8
    , fhLength   :: Int24
    , fhStreamId :: Word32
    } deriving (Show, Eq)

data RawFrame = RawFrame
    { _frameHeader  :: FrameHeader
    , _framePayload :: ByteString
    } deriving (Show, Eq)

data Frame = DataFrame ByteString
           | HeaderFrame (Maybe Exclusive)
                         (Maybe StreamDependency)
                         (Maybe Weight)
                         HeaderBlockFragment
           | PriorityFrame Exclusive StreamDependency Weight
           | RSTStreamFrame RSTStreamErrorCode
           | SettingsFrame SettingsMap
           | PushPromiseFrame PromisedStreamId HeaderBlockFragment
           | PingFrame ByteString
           | GoAwayFrame LastStreamId ErrorCode ByteString
           | WindowUpdateFrame
           | ContinuationFrame
           | UnknownFrame

settingIdToWord16 :: SettingID -> Word16
settingIdToWord16 SettingHeaderTableSize      = 0x1
settingIdToWord16 SettingEnablePush           = 0x2
settingIdToWord16 SettingMaxConcurrentStreams = 0x3
settingIdToWord16 SettingInitialWindowSize    = 0x4

settingIdFromWord16 :: Word16 -> SettingID
settingIdFromWord16 k =
    Map.findWithDefault SettingUnknown k m
  where
    m = Map.fromList $ map (\s -> (settingIdToWord16 s, s)) [minBound..maxBound]

frameTypeToWord8 :: FrameType -> Word8
frameTypeToWord8 FrameData         = 0x0
frameTypeToWord8 FrameHeaders      = 0x1
frameTypeToWord8 FramePriority     = 0x2
frameTypeToWord8 FrameRSTStream    = 0x3
frameTypeToWord8 FrameSettings     = 0x4
frameTypeToWord8 FramePushPromise  = 0x5
frameTypeToWord8 FramePing         = 0x6
frameTypeToWord8 FrameGoAway       = 0x7
frameTypeToWord8 FrameWindowUpdate = 0x8
frameTypeToWord8 FrameContinuation = 0x9

frameTypeFromWord8 :: Word8 -> FrameType
frameTypeFromWord8 k =
    Map.findWithDefault FrameUnknown k m
  where
    m = Map.fromList $ map (\f -> (frameTypeToWord8 f, f)) [minBound..maxBound]

parseFrameHeader :: B.Parser FrameHeader
parseFrameHeader = do
    a <- fromIntegral <$> BI.anyWord16be
    b <- B.anyWord8
    let length = (a `shiftL` 8) .|. fromIntegral b :: Int24
    typ <- frameTypeFromWord8 <$> B.anyWord8
    flags <- B.anyWord8
    streamId <- (`clearBit` 31) <$> BI.anyWord32be
    return $ FrameHeader typ flags length streamId

parseRawFrame :: B.Parser RawFrame
parseRawFrame = do
    header <- parseFrameHeader
    payload <- B.take $ fromIntegral $ fhLength header
    return $ RawFrame header payload

-- | Helper function to pull off the padding if its there, and will
-- eat up the trailing padding automatically. Calls the parser func
-- passed in with the length of the unpadded portion between the
-- padding octet and the actual padding
paddingParser :: FrameHeader -> (Int -> B.Parser a) -> B.Parser a
paddingParser header p =
    if padded then do
        padding <- fromIntegral <$> B.anyWord8
        val <- p $ frameLen - padding - 1
        void $ B.take padding
        return val
    else p frameLen
  where
    flags = fhFlags header
    frameLen = fromIntegral $ fhLength header
    padded = testBit flags 4

parseDataFrame :: FrameParser
parseDataFrame header = paddingParser header $ \len ->
    B.take len >>= return . DataFrame

parseHeadersFrame :: FrameParser
parseHeadersFrame header = paddingParser header $ \len ->
    if priority then do
        eAndStream <- BI.anyWord32be
        weight <- Just . (+1) . fromIntegral <$> B.anyWord8
        let excl = Just $ testBit eAndStream 31
            stream = Just . fromIntegral $ clearBit eAndStream 31
        d <- B.take $ len - 5
        return $ HeaderFrame excl stream weight d
    else do
        B.take len >>= return . (HeaderFrame Nothing Nothing Nothing)
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
parseRstStreamFrame _ = BI.anyWord32be >>= return . RSTStreamFrame

parseSettingsFrame :: FrameParser
parseSettingsFrame header = do
    when (frameLen `mod` 6 /= 0) $ fail "Incorrect frame length"
    settings <- replicateM (frameLen `div` 6) $ do
        rawSetting <- BI.anyWord16be
        let settingId = settingIdFromWord16 rawSetting
        case settingId of
            SettingUnknown -> fail "Invalid settingID"
            s -> (s,) <$> BI.anyWord32be
    return $ SettingsFrame (Map.fromList settings)
  where
    frameLen = fromIntegral $ fhLength header

parsePushPromiseFrame :: FrameParser
parsePushPromiseFrame header = paddingParser header $ \len -> do
    rAndStreamId <- BI.anyWord32be
    let streamId = fromIntegral $ clearBit rAndStreamId 31
    hbf <- B.take $ len - 4
    return $ PushPromiseFrame streamId hbf

parsePingFrame :: FrameParser
parsePingFrame _ = B.take 8 >>= return . PingFrame

parseGoAwayFrame :: FrameParser
parseGoAwayFrame header = do
    rAndLastStreamId <- BI.anyWord32be
    errCode <- errorCodeFromWord32 <$> BI.anyWord32be
    debug <- B.take $ frameLen - 8
    let streamId = fromIntegral $ clearBit rAndLastStreamId 31
    case errCode of
        Nothing -> fail "Invalid error code"
        Just err -> return $ GoAwayFrame streamId err debug
  where
    frameLen = fromIntegral $ fhLength header
