module Network.HTTP2.Types where

import Data.ByteString (ByteString)
import qualified Data.Map as Map -- FIXME
import Data.Word (Word8, Word16, Word32)

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
-- Just NoError
-- >>> errorCodeFromWord32 0xc
-- Just InadequateSecurity
-- >>> errorCodeFromWord32 0xd
-- Nothing
errorCodeFromWord32 :: Word32 -> Maybe ErrorCode
errorCodeFromWord32 x
  | minErrorCode <= x && x <= maxErrorCode = Just . toEnum . fromIntegral $ x
  | otherwise                              = Nothing

----------------------------------------------------------------

data Settings = SettingsHeaderTableSize
              | SettingsEnablePush
              | SettingsMaxConcurrentStreams
              | SettingsInitialWindowSize
              | SettingsMaxFrameSize
              | SettingsMaxHeaderBlockSize
              deriving (Show, Eq, Ord, Enum, Bounded)

-- |
--
-- >>> settingsToWord16 SettingsHeaderTableSize
-- 1
-- >>> settingsToWord16 SettingsMaxHeaderBlockSize
-- 6
settingsToWord16 :: Settings -> Word16
settingsToWord16 x = fromIntegral (fromEnum x) + 1

minSettings :: Word16
minSettings = fromIntegral $ fromEnum (minBound :: Settings)

maxSettings :: Word16
maxSettings = fromIntegral $ fromEnum (maxBound :: Settings)

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
settingsFromWord16 :: Word16 -> Maybe Settings
settingsFromWord16 x
  | minSettings <= n && n <= maxSettings = Just . toEnum . fromIntegral $ n
  | otherwise                            = Nothing
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
               deriving (Show, Eq, Ord, Enum, Bounded)

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

type FrameLength         = Int -- Word24 but Int is more natural
type FrameFlags          = Word8

newtype StreamIdentifier = StreamIdentifier Word32 deriving (Show, Eq)
type StreamDependency    = StreamIdentifier
type LastStreamId        = StreamIdentifier
type PromisedStreamId    = StreamIdentifier
type WindowSizeIncrement = StreamIdentifier

streamIdentifierForSeetings :: StreamIdentifier
streamIdentifierForSeetings = StreamIdentifier 0

type HeaderBlockFragment = ByteString
type Exclusive           = Bool
type Weight              = Int

-- A complete frame header
data FrameHeader = FrameHeader
    { fhLength   :: FrameLength
    , fhType     :: FrameType
    , fhFlags    :: FrameFlags
    , fhStreamId :: StreamIdentifier
    } deriving (Show, Eq)

-- The raw frame is the header with the payload body, but not a parsed
-- full frame
data RawFrame = RawFrame
    { frameHeader  :: FrameHeader
    , framePayload :: ByteString
    } deriving (Show, Eq)

data Frame = DataFrame ByteString
           | HeaderFrame (Maybe Exclusive)
                         (Maybe StreamDependency)
                         (Maybe Weight)
                         HeaderBlockFragment
           | PriorityFrame Exclusive StreamDependency Weight
           | RSTStreamFrame ErrorCode
           | SettingsFrame SettingsMap
           | PushPromiseFrame PromisedStreamId HeaderBlockFragment
           | PingFrame ByteString
           | GoAwayFrame LastStreamId ErrorCode ByteString
           | WindowUpdateFrame WindowSizeIncrement
           | ContinuationFrame HeaderBlockFragment
           | UnknownFrame ByteString

-- Valid settings map
type SettingsMap = Map.Map Settings Word32 -- fixme
