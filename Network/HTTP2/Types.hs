module Network.HTTP2.Types where

import Data.Array (Array, Ix, listArray)
import Data.Array.ST (newArray, writeArray, runSTArray)
import Data.ByteString (ByteString)
import Data.Word (Word8, Word16, Word32)
import Control.Monad (forM_)

----------------------------------------------------------------

data ErrorCode = NoError
               | ProtocolError
               | InternalError
               | FlowControlError
               | SettingsTimeout
               | StreamClosed
               | FrameSizeError
               | RefusedStream
               | Cancel
               | CompressionError
               | ConnectError
               | EnhanceYourCalm
               | InadequateSecurity
               deriving (Show, Eq, Ord, Enum, Bounded)

-- |
--
-- >>> errorCodeToWord32 NoError
-- 0
-- >>> errorCodeToWord32 InadequateSecurity
-- 12
errorCodeToWord32 :: ErrorCode -> Word32
errorCodeToWord32 = fromIntegral . fromEnum

minErrorCode :: Word32
minErrorCode = fromIntegral $ fromEnum (minBound :: ErrorCode)

maxErrorCode :: Word32
maxErrorCode = fromIntegral $ fromEnum (maxBound :: ErrorCode)

-- |
--
-- >>> errorCodeFromWord32 0
-- Right NoError
-- >>> errorCodeFromWord32 0xc
-- Right InadequateSecurity
-- >>> errorCodeFromWord32 0xd
-- Left 13
errorCodeFromWord32 :: Word32 -> Either Word32 ErrorCode
errorCodeFromWord32 x
  | minErrorCode <= x && x <= maxErrorCode = Right . toEnum . fromIntegral $ x
  | otherwise                              = Left x

----------------------------------------------------------------

data SettingsId = SettingsHeaderTableSize
                | SettingsEnablePush
                | SettingsMaxConcurrentStreams
                | SettingsInitialWindowSize
                | SettingsMaxFrameSize -- this means payload size
                | SettingsMaxHeaderBlockSize
                deriving (Show, Eq, Ord, Enum, Bounded, Ix)

-- |
--
-- >>> settingsToWord16 SettingsHeaderTableSize
-- 1
-- >>> settingsToWord16 SettingsMaxHeaderBlockSize
-- 6
settingsToWord16 :: SettingsId -> Word16
settingsToWord16 x = fromIntegral (fromEnum x) + 1

minSettingsId :: Word16
minSettingsId = fromIntegral $ fromEnum (minBound :: SettingsId)

maxSettingsId :: Word16
maxSettingsId = fromIntegral $ fromEnum (maxBound :: SettingsId)

-- |
--
-- >>> settingsFromWord16 0
-- Nothing
-- >>> settingsFromWord16 1
-- Just SettingsHeaderTableSize
-- >>> settingsFromWord16 6
-- Just SettingsMaxHeaderBlockSize
-- >>> settingsFromWord16 7
-- Nothing
settingsFromWord16 :: Word16 -> Maybe SettingsId
settingsFromWord16 x
  | minSettingsId <= n && n <= maxSettingsId = Just . toEnum . fromIntegral $ n
  | otherwise                                = Nothing
  where
    n = x - 1

----------------------------------------------------------------

-- Valid frame types
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
               deriving (Show, Eq, Ord, Enum, Bounded, Ix)

-- |
--
-- >>> frameTypeToWord8 FrameData
-- 0
-- >>> frameTypeToWord8 FrameContinuation
-- 9
frameTypeToWord8 :: FrameType -> Word8
frameTypeToWord8 = fromIntegral . fromEnum

minFrameType :: Word8
minFrameType = fromIntegral $ fromEnum (minBound :: FrameType)

maxFrameType :: Word8
maxFrameType = fromIntegral $ fromEnum (maxBound :: FrameType)

-- |
--
-- >>> frameTypeFromWord8 0
-- Just FrameData
-- >>> frameTypeFromWord8 9
-- Just FrameContinuation
-- >>> frameTypeFromWord8 10
-- Nothing
frameTypeFromWord8 :: Word8 -> Maybe FrameType
frameTypeFromWord8 x
  | minFrameType <= x && x <= maxFrameType = Just . toEnum . fromIntegral $ x
  | otherwise                              = Nothing

----------------------------------------------------------------

type PayloadLength = Int -- Word24 but Int is more natural

maxPayloadLength :: PayloadLength
maxPayloadLength = 2^(14::Int)

type FrameFlags          = Word8

newtype StreamIdentifier = StreamIdentifier Word32 deriving (Show, Eq)
type StreamDependency    = StreamIdentifier
type LastStreamId        = StreamIdentifier
type PromisedStreamId    = StreamIdentifier
type WindowSizeIncrement = StreamIdentifier

fromStreamIdentifier :: StreamIdentifier -> Word32
fromStreamIdentifier (StreamIdentifier w32) = w32

streamIdentifierForSeetings :: StreamIdentifier
streamIdentifierForSeetings = StreamIdentifier 0

type HeaderBlockFragment = ByteString

data Frame = Frame
    { frameHeader  :: FrameHeader
    , framePayload :: FramePayload
    } deriving (Show, Eq)

-- A complete frame header
data FrameHeader = FrameHeader
    { payloadLength :: PayloadLength
    , frameType     :: FrameType
    , flags         :: FrameFlags
    , streamId      :: StreamIdentifier
    } deriving (Show, Eq)

data FramePayload =
    DataFrame ByteString
  | HeaderFrame (Maybe Priority) HeaderBlockFragment
  | PriorityFrame Priority
  | RSTStreamFrame (Either Word32 ErrorCode)
  | SettingsFrame Settings
  | PushPromiseFrame PromisedStreamId HeaderBlockFragment
  | PingFrame ByteString
  | GoAwayFrame LastStreamId (Either Word32 ErrorCode) ByteString
  | WindowUpdateFrame WindowSizeIncrement
  | ContinuationFrame HeaderBlockFragment
  deriving (Show, Eq)

type Settings = Array SettingsId (Maybe Word32)

defaultSettings :: Settings
defaultSettings = listArray settingsRange [Nothing|_<-xs]
  where
    xs = [minBound :: SettingsId .. maxBound :: SettingsId]

-- |
--
-- >>> toSettings [(SettingsHeaderTableSize,10),(SettingsInitialWindowSize,20),(SettingsHeaderTableSize,30)]
-- array (SettingsHeaderTableSize,SettingsMaxHeaderBlockSize) [(SettingsHeaderTableSize,Just 30),(SettingsEnablePush,Nothing),(SettingsMaxConcurrentStreams,Nothing),(SettingsInitialWindowSize,Just 20),(SettingsMaxFrameSize,Nothing),(SettingsMaxHeaderBlockSize,Nothing)]
toSettings :: [(SettingsId,Word32)] -> Settings
toSettings kvs = runSTArray $ do
    arr <- newArray settingsRange Nothing
    forM_ kvs $ \(k,v) -> writeArray arr k (Just v)
    return arr

settingsRange :: (SettingsId, SettingsId)
settingsRange = (minBound, maxBound)

data Priority = Priority {
    exclusive :: Bool
  , streamDependency :: StreamIdentifier
  , weight :: Int
  } deriving (Eq, Show)
