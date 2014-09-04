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

import Control.Applicative ((<$>))
import Control.Monad (void, (>=>), when)
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

-- Error code is encoded in String.
-- We can convert it to 'Either ErrorCode Frame'.
parseFrame :: Settings -> B.Parser Frame
parseFrame settings = do
    header <- parseFrameHeader settings
    Frame header <$> parseFramePayload header

----------------------------------------------------------------

parseFrameHeader :: Settings -> B.Parser FrameHeader
parseFrameHeader settings = do
    w16 <- fromIntegral <$> BI.anyWord16be
    w8 <- fromIntegral <$> B.anyWord8
    let len = (w16 `shiftL` 8) .|. w8
    when (doesExceed settings len) $ fail frameSizeError
    tp <- B.anyWord8
    let mtyp = frameTypeFromWord8 tp
    case mtyp of
        Nothing  -> do
            ignore len
            fail noError
        Just typ -> do
            flags <- B.anyWord8
            (streamId, _) <- streamIdentifier
            when (isProtocolError settings typ streamId) $ fail protocolError
            return $ FrameHeader len typ flags streamId

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
isProtocolError settings typ streamId
  | typ `elem` nonZeroFrameTypes && streamId == streamIdentifierForSeetings = True
  | typ `elem` zeroFrameTypes && streamId /= streamIdentifierForSeetings = True
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
    parsePayload = payloadParsers ! fhType header

----------------------------------------------------------------

parseDataFrame :: FramePayloadParser
parseDataFrame header = parseWithPadding header $ B.take >=> (return . DataFrame)

parseHeadersFrame :: FramePayloadParser
parseHeadersFrame header = parseWithPadding header $ \len ->
    if priority then do
        (streamId, excl) <- streamIdentifier
        weight <- (+1) . fromIntegral <$> B.anyWord8
        d <- B.take $ len - 5
        return $ HeaderFrame (Just excl) (Just streamId) (Just weight) d
    else
        HeaderFrame Nothing Nothing Nothing <$> B.take len
  where
    priority = testPriority $ fhFlags header

parsePriorityFrame :: FramePayloadParser
parsePriorityFrame _ = do
    (streamId, excl) <- streamIdentifier
    weight <- (+1) . fromIntegral <$> B.anyWord8
    return $ PriorityFrame excl streamId weight

parseRstStreamFrame :: FramePayloadParser
parseRstStreamFrame _ = RSTStreamFrame . errorCodeFromWord32 <$> BI.anyWord32be

parseSettingsFrame :: FramePayloadParser
parseSettingsFrame FrameHeader{..}
  | isNotValid = fail protocolError
  | otherwise  = SettingsFrame <$> settings num id
  where
    num = fhLength `div` 6
    isNotValid = fhLength `mod` 6 /= 0
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
parsePushPromiseFrame header = parseWithPadding header $ \len -> do
    (streamId, _) <- streamIdentifier
    hbf <- B.take $ len - 4
    return $ PushPromiseFrame streamId hbf

parsePingFrame :: FramePayloadParser
parsePingFrame header
  | frameLen header /= 8 = fail frameSizeError
  | otherwise            = PingFrame <$> B.take 8

parseGoAwayFrame :: FramePayloadParser
parseGoAwayFrame header = do
    (streamId, _) <- streamIdentifier
    errCode <- errorCodeFromWord32 <$> BI.anyWord32be
    debug <- B.take $ frameLen header - 8
    return $ GoAwayFrame streamId errCode debug

parseWindowUpdateFrame :: FramePayloadParser
parseWindowUpdateFrame header
  | frameLen header /= 4 = fail frameSizeError -- not sure
  | otherwise            = do
      (streamId, _) <- streamIdentifier
      return $ WindowUpdateFrame streamId

parseContinuationFrame :: FramePayloadParser
parseContinuationFrame header = ContinuationFrame <$> B.take (frameLen header)

----------------------------------------------------------------

-- | Helper function to pull off the padding if its there, and will
-- eat up the trailing padding automatically. Calls the parser func
-- passed in with the length of the unpadded portion between the
-- padding octet and the actual padding
parseWithPadding :: FrameHeader -> (Int -> B.Parser a) -> B.Parser a
parseWithPadding header p
  | padded = do
      padding <- fromIntegral <$> B.anyWord8
      val <- p $ frameLen header - padding - 1 -- fixme: -1?
      ignore padding
      return val
  | otherwise = p $ frameLen header
  where
    padded = testPadded $ fhFlags header

streamIdentifier :: B.Parser (StreamIdentifier, Bool)
streamIdentifier = do
    w32 <- BI.anyWord32be
    let !streamdId = StreamIdentifier $ clearBit w32 31
        !exclusive = testBit w32 31
    return (streamdId, exclusive)

frameLen :: FrameHeader -> Int
frameLen h = fromIntegral $ fhLength h

ignore :: Int -> B.Parser ()
ignore n = void $ B.take n
