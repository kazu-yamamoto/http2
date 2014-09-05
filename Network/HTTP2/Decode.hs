{-# LANGUAGE TupleSections, BangPatterns, RecordWildCards #-}

module Network.HTTP2.Decode (
    decodeFrame
  , decodeFrameHeader
  , parseFrame
  , parseFrameHeader
  , parseFramePayload
  , testEndStream
  , testAck
  , testEndHeader
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (void, when)
import Data.Array (Array, listArray, (!))
import qualified Data.Attoparsec.Binary as BI
import qualified Data.Attoparsec.ByteString as B
import Data.Bits (clearBit, shiftL, testBit, (.|.))
import Data.ByteString (ByteString)

import Network.HTTP2.Types

----------------------------------------------------------------
-- atto-parsec can return only String as an error type, sigh.

-- This is a special case: frame type is unknown
-- and its frame is just ignored.
noError :: String
noError = show noError

protocolError :: String
protocolError = show ProtocolError

frameSizeError :: String
frameSizeError = show FrameSizeError

----------------------------------------------------------------

decodeFrame :: Settings -> ByteString -> Either ErrorCode Frame
decodeFrame settings bs = case B.parseOnly (parseFrame settings) bs of
    Right frame -> Right frame
    Left  estr  -> Left $ toErrorCode estr

decodeFrameHeader :: Settings -> ByteString -> Either ErrorCode FrameHeader
decodeFrameHeader settings bs = case B.parseOnly (parseFrameHeader settings) bs of
    Right fh   -> Right fh
    Left  estr -> Left $ toErrorCode estr

toErrorCode :: String -> ErrorCode
toErrorCode estr
  | estr == protocolError  = ProtocolError
  | estr == frameSizeError = FrameSizeError
  | otherwise              = NoError

----------------------------------------------------------------

parseFrame :: Settings -> B.Parser Frame
parseFrame settings = do
    header <- parseFrameHeader settings
    Frame header <$> parseFramePayload header

----------------------------------------------------------------

parseFrameHeader :: Settings -> B.Parser FrameHeader
parseFrameHeader settings = do
    i0 <- intFromWord16be
    i1 <- intFromWord8
    let len = (i0 `shiftL` 8) .|. i1
    when (doesExceed settings len) $ fail frameSizeError
    tp <- B.anyWord8
    let mtyp = frameTypeFromWord8 tp
    case mtyp of
        Nothing  -> do
            ignore len
            fail noError
        Just typ -> do
            flg <- B.anyWord8
            sid <- streamIdentifier
            when (isProtocolError settings typ sid) $ fail protocolError
            return $ FrameHeader len typ flg sid

doesExceed :: Settings -> PayloadLength -> Bool
doesExceed settings len = len > maxLength
  where
    maxLength = case settings ! SettingsMaxFrameSize of
        Just x  -> fromIntegral x
        Nothing -> maxPayloadLength

zeroFrameTypes :: [FrameType]
zeroFrameTypes = [
    FrameSettings
  , FramePing
  , FrameGoAway
  ]

nonZeroFrameTypes :: [FrameType]
nonZeroFrameTypes = [
    FrameData
  , FrameHeaders
  , FramePriority
  , FrameRSTStream
  , FramePushPromise
  , FrameContinuation
  ]

isProtocolError :: Settings -> FrameType -> StreamIdentifier -> Bool
isProtocolError settings typ sid
  | typ `elem` nonZeroFrameTypes && sid == streamIdentifierForSeetings = True
  | typ `elem` zeroFrameTypes && sid /= streamIdentifierForSeetings = True
  | typ == FramePushPromise && not pushEnabled = True
  | otherwise = False
  where
    pushEnabled = case settings ! SettingsEnablePush of
        Nothing -> True
        Just x  -> x /= 0

----------------------------------------------------------------

type FramePayloadParser = FrameHeader -> B.Parser FramePayload

payloadParsers :: Array FrameType FramePayloadParser
payloadParsers = listArray (minBound :: FrameType, maxBound :: FrameType)
    [ parseDataFrame
    , parseHeadersFrame
    , parsePriorityFrame
    , parseRstStreamFrame
    , parseSettingsFrame
    , parsePushPromiseFrame
    , parsePingFrame
    , parseGoAwayFrame
    , parseWindowUpdateFrame
    , parseContinuationFrame
    ]

parseFramePayload :: FramePayloadParser
parseFramePayload header = parsePayload header
  where
    -- header always contain a valid FrameType
    parsePayload = payloadParsers ! frameType header

----------------------------------------------------------------

parseDataFrame :: FramePayloadParser
parseDataFrame header = parseWithPadding header $ \len ->
    DataFrame <$> B.take len

parseHeadersFrame :: FramePayloadParser
parseHeadersFrame header = parseWithPadding header $ \len ->
    if hasPriority then do
        p <- priority
        HeaderFrame (Just p) <$> B.take (len - 5)
    else
        HeaderFrame Nothing <$> B.take len
  where
    hasPriority = testPriority $ flags header

parsePriorityFrame :: FramePayloadParser
parsePriorityFrame _ = PriorityFrame <$> priority

parseRstStreamFrame :: FramePayloadParser
parseRstStreamFrame _ = RSTStreamFrame . errorCodeFromWord32 <$> BI.anyWord32be

parseSettingsFrame :: FramePayloadParser
parseSettingsFrame FrameHeader{..}
  | isNotValid = fail protocolError
  | otherwise  = SettingsFrame <$> settings num id
  where
    num = payloadLength `div` 6
    isNotValid = payloadLength `mod` 6 /= 0
    settings 0 builder = return $ toSettings $ builder []
    settings n builder = do
        rawSetting <- BI.anyWord16be
        let msettings = settingsFromWord16 rawSetting
            n' = n - 1
        case msettings of
            Nothing -> settings n' builder -- ignoring unknown one (Section 6.5.2)
            Just k  -> do
                v <- BI.anyWord32be
                settings n' (((k,v):) . builder)

parsePushPromiseFrame :: FramePayloadParser
parsePushPromiseFrame header = parseWithPadding header $ \len ->
    PushPromiseFrame <$> streamIdentifier <*> hbf len
  where
    hbf len = B.take $ len - 4

parsePingFrame :: FramePayloadParser
parsePingFrame FrameHeader{..}
  | payloadLength /= 8 = fail frameSizeError
  | otherwise          = PingFrame <$> B.take 8

parseGoAwayFrame :: FramePayloadParser
parseGoAwayFrame FrameHeader{..} =
    GoAwayFrame <$> streamIdentifier <*> errCode <*> debug
  where
    errCode = errorCodeFromWord32 <$> BI.anyWord32be
    debug = B.take $ payloadLength - 8

parseWindowUpdateFrame :: FramePayloadParser
parseWindowUpdateFrame FrameHeader{..}
  | payloadLength /= 4 = fail frameSizeError -- not sure
  | otherwise          = WindowUpdateFrame <$> streamIdentifier

parseContinuationFrame :: FramePayloadParser
parseContinuationFrame FrameHeader{..} = ContinuationFrame <$> payload
  where
    payload = B.take payloadLength

----------------------------------------------------------------

-- | Helper function to pull off the padding if its there, and will
-- eat up the trailing padding automatically. Calls the parser func
-- passed in with the length of the unpadded portion between the
-- padding octet and the actual padding
parseWithPadding :: FrameHeader -> (Int -> B.Parser a) -> B.Parser a
parseWithPadding FrameHeader{..} p
  | padded = do
      padlen <- intFromWord8
      -- padding length consumes 1 byte.
      val <- p $ payloadLength - padlen - 1
      ignore padlen
      return val
  | otherwise = p payloadLength
  where
    padded = testPadded flags

streamIdentifier :: B.Parser StreamIdentifier
streamIdentifier = StreamIdentifier . (`clearBit` 31) <$> BI.anyWord32be

streamIdentifier' :: B.Parser (StreamIdentifier, Bool)
streamIdentifier' = do
    w32 <- BI.anyWord32be
    let !streamdId = StreamIdentifier $ clearBit w32 31
        !exclusive = testBit w32 31
    return (streamdId, exclusive)

priority :: B.Parser Priority
priority = do
    (sid, excl) <- streamIdentifier'
    Priority excl sid <$> w
  where
    w = (+1) <$> intFromWord8

ignore :: Int -> B.Parser ()
ignore n = void $ B.take n

intFromWord8 :: B.Parser Int
intFromWord8 = fromIntegral <$> B.anyWord8

intFromWord16be :: B.Parser Int
intFromWord16be = fromIntegral <$> BI.anyWord16be

----------------------------------------------------------------
-- Flags

-- |
-- >>> testEndStream 0x1
-- True
testEndStream :: FrameFlags -> Bool
testEndStream x = x `testBit` 0

-- |
-- >>> testAck 0x1
-- True
testAck :: FrameFlags -> Bool
testAck x = x `testBit` 0 -- fixme: is the spec intentional?

-- |
-- >>> testEndHeader 0x4
-- True
testEndHeader :: FrameFlags -> Bool
testEndHeader x = x `testBit` 2

-- |
-- >>> testPadded 0x8
-- True
testPadded :: FrameFlags -> Bool
testPadded x = x `testBit` 3

-- |
-- >>> testPriority 0x20
-- True
testPriority :: FrameFlags -> Bool
testPriority x = x `testBit` 5
