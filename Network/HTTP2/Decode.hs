{-# LANGUAGE TupleSections, BangPatterns, RecordWildCards #-}

module Network.HTTP2.Decode (
    decodeFrame
  ) where

import Control.Applicative ((<$>))
import Control.Monad (void, (>=>), when)
import Data.Array (Array, listArray, (!))
import qualified Data.Attoparsec.Binary as BI
import qualified Data.Attoparsec.ByteString as B
import Data.Bits (clearBit, shiftL, testBit, (.|.))

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

-- Error code is encoded in String.
-- We can convert it to 'Either ErrorCode Frame'.
decodeFrame :: Settings -> B.Parser Frame
decodeFrame settings = do
    header <- decodeFrameHeader settings
    Frame header <$> decodeFramePayload header

----------------------------------------------------------------

decodeFrameHeader :: Settings -> B.Parser FrameHeader
decodeFrameHeader settings = do
    a <- fromIntegral <$> BI.anyWord16be
    b <- B.anyWord8
    let frameLength = (a `shiftL` 8) .|. fromIntegral b :: FrameLength
    when (frameLength > maxSize) $ fail frameSizeError
    tp <- B.anyWord8
    let mtyp = frameTypeFromWord8 tp
    case mtyp of
        Nothing  -> do
            void $ B.take frameLength
            fail noError
        Just typ -> do
            flags <- B.anyWord8
            (streamId, _) <- streamIdentifier
            when (typ `elem` nonZeroFrameTypes &&
                  streamId == streamIdentifierForSeetings) $
                fail protocolError
            when (typ `elem` zeroFrameTypes &&
                  streamId /= streamIdentifierForSeetings) $
                fail protocolError
            when (typ == FramePushPromise && not pushEnabled) $
                fail protocolError
            return $ FrameHeader frameLength typ flags streamId
  where
    maxSize = case settings ! SettingsMaxFrameSize of
        Just x  -> fromIntegral x
        Nothing -> maxFrameSize
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
    pushEnabled = case settings ! SettingsEnablePush of
        Nothing -> True
        Just x  -> x /= 0

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

decodePushPromiseFrame :: FramePayloadDecoder
decodePushPromiseFrame header = decodeWithPadding header $ \len -> do
    (streamId, _) <- streamIdentifier
    hbf <- B.take $ len - 4
    return $ PushPromiseFrame streamId hbf

decodePingFrame :: FramePayloadDecoder
decodePingFrame header
  | frameLen header /= 8 = fail frameSizeError
  | otherwise            = PingFrame <$> B.take 8

decodeGoAwayFrame :: FramePayloadDecoder
decodeGoAwayFrame header = do
    (streamId, _) <- streamIdentifier
    errCode <- errorCodeFromWord32 <$> BI.anyWord32be
    debug <- B.take $ frameLen header - 8
    return $ GoAwayFrame streamId errCode debug

decodeWindowUpdateFrame :: FramePayloadDecoder
decodeWindowUpdateFrame header
  | frameLen header /= 4 = fail frameSizeError -- not sure
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
