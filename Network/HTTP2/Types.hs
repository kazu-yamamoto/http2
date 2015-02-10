{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2.Types (
  -- * Constant
    frameHeaderLength
  , maxPayloadLength
  -- * SettingsList
  , SettingsKeyId(..)
  , checkSettingsList
  , fromSettingsKeyId
  , SettingsValue
  , SettingsList
  , toSettingsKeyId
  -- * Settings
  , Settings(..)
  , defaultSettings
  , updateSettings
  -- * Error
  , HTTP2Error(..)
  , errorCodeId
  -- * Error code
  , ErrorCode
  , ErrorCodeId(..)
  , fromErrorCodeId
  , toErrorCodeId
  -- * Frame type
  , FrameType
  , minFrameType
  , maxFrameType
  , FrameTypeId(..)
  , fromFrameTypeId
  , toFrameTypeId
  -- * Frame
  , Frame(..)
  , FrameHeader(..)
  , FramePayload(..)
  , framePayloadToFrameTypeId
  -- * Stream identifier
  , StreamIdentifier(..)
  , fromStreamIdentifier
  , toStreamIdentifier
  , isControl
  , isRequest
  , isResponse
  , PromisedStreamId
  , LastStreamId
  , StreamDependency
  , testExclusive
  , setExclusive
  -- * Flags
  , FrameFlags
  , defaultFlags
  , testEndStream
  , testAck
  , testEndHeader
  , testPadded
  , testPriority
  , setEndStream
  , setAck
  , setEndHeader
  , setPadded
  , setPriority
  -- * Types
  , WindowSizeIncrement
  , HeaderBlockFragment
  , Priority(..)
  , Padding
  ) where

import qualified Control.Exception as E
import Data.Bits (setBit, testBit, clearBit)
import Data.ByteString (ByteString)
import Data.Maybe (mapMaybe)
import Data.Typeable
import Data.Word (Word8, Word16, Word32)

----------------------------------------------------------------

-- | The length of HTTP/2 frame header.
--
-- >>> frameHeaderLength
-- 9
frameHeaderLength :: Int
frameHeaderLength = 9

----------------------------------------------------------------

type ErrorCode = Word32

data ErrorCodeId = NoError
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
                 | HTTP11Required
                   -- our extensions
                 | UnknownErrorCode ErrorCode
                 | UnknownError String
                 deriving (Show, Read, Eq, Ord)

-- |
--
-- >>> fromErrorCodeId NoError
-- 0
-- >>> fromErrorCodeId InadequateSecurity
-- 12
fromErrorCodeId :: ErrorCodeId -> ErrorCode
fromErrorCodeId NoError              = 0x0
fromErrorCodeId ProtocolError        = 0x1
fromErrorCodeId InternalError        = 0x2
fromErrorCodeId FlowControlError     = 0x3
fromErrorCodeId SettingsTimeout      = 0x4
fromErrorCodeId StreamClosed         = 0x5
fromErrorCodeId FrameSizeError       = 0x6
fromErrorCodeId RefusedStream        = 0x7
fromErrorCodeId Cancel               = 0x8
fromErrorCodeId CompressionError     = 0x9
fromErrorCodeId ConnectError         = 0xa
fromErrorCodeId EnhanceYourCalm      = 0xb
fromErrorCodeId InadequateSecurity   = 0xc
fromErrorCodeId HTTP11Required       = 0xd
fromErrorCodeId (UnknownErrorCode w) = w
fromErrorCodeId _                    = 255 -- never reached

-- |
--
-- >>> toErrorCodeId 0
-- NoError
-- >>> toErrorCodeId 0xc
-- InadequateSecurity
-- >>> toErrorCodeId 0xe
-- UnknownErrorCode 14
toErrorCodeId :: ErrorCode -> ErrorCodeId
toErrorCodeId 0x0 = NoError
toErrorCodeId 0x1 = ProtocolError
toErrorCodeId 0x2 = InternalError
toErrorCodeId 0x3 = FlowControlError
toErrorCodeId 0x4 = SettingsTimeout
toErrorCodeId 0x5 = StreamClosed
toErrorCodeId 0x6 = FrameSizeError
toErrorCodeId 0x7 = RefusedStream
toErrorCodeId 0x8 = Cancel
toErrorCodeId 0x9 = CompressionError
toErrorCodeId 0xa = ConnectError
toErrorCodeId 0xb = EnhanceYourCalm
toErrorCodeId 0xc = InadequateSecurity
toErrorCodeId 0xd = HTTP11Required
toErrorCodeId w   = UnknownErrorCode w

----------------------------------------------------------------

-- | The connection error or the stream error.
data HTTP2Error = ConnectionError ErrorCodeId ByteString
                | StreamError ErrorCodeId StreamIdentifier
                deriving (Eq, Show, Typeable, Read)

instance E.Exception HTTP2Error

-- | Obtaining 'ErrorCodeId' from 'HTTP2Error'.
errorCodeId :: HTTP2Error -> ErrorCodeId
errorCodeId (ConnectionError err _) = err
errorCodeId (StreamError     err _) = err

----------------------------------------------------------------

data SettingsKeyId = SettingsHeaderTableSize
                   | SettingsEnablePush
                   | SettingsMaxConcurrentStreams
                   | SettingsInitialWindowSize
                   | SettingsMaxFrameSize -- this means payload size
                   | SettingsMaxHeaderBlockSize
                   deriving (Show, Read, Eq, Ord, Enum, Bounded)

type SettingsValue = Int -- Word32

-- |
--
-- >>> fromSettingsKeyId SettingsHeaderTableSize
-- 1
-- >>> fromSettingsKeyId SettingsMaxHeaderBlockSize
-- 6
fromSettingsKeyId :: SettingsKeyId -> Word16
fromSettingsKeyId x = fromIntegral (fromEnum x) + 1

minSettingsKeyId :: Word16
minSettingsKeyId = fromIntegral $ fromEnum (minBound :: SettingsKeyId)

maxSettingsKeyId :: Word16
maxSettingsKeyId = fromIntegral $ fromEnum (maxBound :: SettingsKeyId)

-- |
--
-- >>> toSettingsKeyId 0
-- Nothing
-- >>> toSettingsKeyId 1
-- Just SettingsHeaderTableSize
-- >>> toSettingsKeyId 6
-- Just SettingsMaxHeaderBlockSize
-- >>> toSettingsKeyId 7
-- Nothing
toSettingsKeyId :: Word16 -> Maybe SettingsKeyId
toSettingsKeyId x
  | minSettingsKeyId <= n && n <= maxSettingsKeyId = Just . toEnum . fromIntegral $ n
  | otherwise                                = Nothing
  where
    n = x - 1

----------------------------------------------------------------

-- | Settings containing raw values.
type SettingsList = [(SettingsKeyId,SettingsValue)]

-- | Checking 'SettingsList' and reporting an error if any.
--
-- >>> checkSettingsList [(SettingsEnablePush,2)]
-- Just (ConnectionError ProtocolError "enable push must be 0 or 1")
checkSettingsList :: SettingsList -> Maybe HTTP2Error
checkSettingsList settings = case mapMaybe checkSettingsValue settings of
    []    -> Nothing
    (x:_) -> Just x

checkSettingsValue :: (SettingsKeyId,SettingsValue) -> Maybe HTTP2Error
checkSettingsValue (SettingsEnablePush,v)
  | v /= 0 && v /= 1 = Just $ ConnectionError ProtocolError "enable push must be 0 or 1"
checkSettingsValue (SettingsInitialWindowSize,v)
  | v > 2147483647   = Just $ ConnectionError FlowControlError "Window size must be less than or equal to 65535"
checkSettingsValue (SettingsMaxFrameSize,v)
  | v < 16384 || v > 16777215 = Just $ ConnectionError ProtocolError "Max frame size must be in between 16384 and 16777215"
checkSettingsValue _ = Nothing

----------------------------------------------------------------

-- | Cooked version of settings. This is suitable to be stored in a HTTP/2 context.
data Settings = Settings {
    headerTableSize :: Int
  , enablePush :: Bool
  , maxConcurrentStreams :: Int
  , initialWindowSize :: Int
  , maxFrameSize :: Int
  , maxHeaderBlockSize :: Maybe Int
  } deriving (Show)

-- | The default settings.
--
-- >>> defaultSettings
-- Settings {headerTableSize = 4096, enablePush = True, maxConcurrentStreams = 100, initialWindowSize = 65535, maxFrameSize = 16384, maxHeaderBlockSize = Nothing}
defaultSettings :: Settings
defaultSettings = Settings {
    headerTableSize = 4096
  , enablePush = True
  , maxConcurrentStreams = 100
  , initialWindowSize = 65535
  , maxFrameSize = 16384
  , maxHeaderBlockSize = Nothing
  }

-- | Updating settings.
--
-- >>> updateSettings defaultSettings [(SettingsEnablePush,0),(SettingsMaxHeaderBlockSize,200)]
-- Settings {headerTableSize = 4096, enablePush = False, maxConcurrentStreams = 100, initialWindowSize = 65535, maxFrameSize = 16384, maxHeaderBlockSize = Just 200}
updateSettings :: Settings -> SettingsList -> Settings
updateSettings settings kvs = foldr update settings kvs
  where
    update (SettingsHeaderTableSize,x)      def = def { headerTableSize = x }
    -- fixme: x should be 0 or 1
    update (SettingsEnablePush,x)           def = def { enablePush = x > 0 }
    update (SettingsMaxConcurrentStreams,x) def = def { maxConcurrentStreams = x }
    update (SettingsInitialWindowSize,x)    def = def { initialWindowSize = x }
    update (SettingsMaxFrameSize,x)         def = def { maxFrameSize = x }
    update (SettingsMaxHeaderBlockSize,x)   def = def { maxHeaderBlockSize = Just x }

----------------------------------------------------------------

data Priority = Priority {
    exclusive :: Bool
  , streamDependency :: StreamIdentifier
  , weight :: Int
  } deriving (Show, Read, Eq)

----------------------------------------------------------------

type FrameType = Word8

minFrameType :: FrameType
minFrameType = 0

maxFrameType :: FrameType
maxFrameType = 9

-- Valid frame types
data FrameTypeId = FrameData
                 | FrameHeaders
                 | FramePriority
                 | FrameRSTStream
                 | FrameSettings
                 | FramePushPromise
                 | FramePing
                 | FrameGoAway
                 | FrameWindowUpdate
                 | FrameContinuation
                 | FrameUnknown FrameType
                 deriving (Show, Eq, Ord)

-- |
--
-- >>> fromFrameTypeId FrameData
-- 0
-- >>> fromFrameTypeId FrameContinuation
-- 9
-- >>> fromFrameTypeId (FrameUnknown 10)
-- 10
fromFrameTypeId :: FrameTypeId -> FrameType
fromFrameTypeId FrameData         = 0
fromFrameTypeId FrameHeaders      = 1
fromFrameTypeId FramePriority     = 2
fromFrameTypeId FrameRSTStream    = 3
fromFrameTypeId FrameSettings     = 4
fromFrameTypeId FramePushPromise  = 5
fromFrameTypeId FramePing         = 6
fromFrameTypeId FrameGoAway       = 7
fromFrameTypeId FrameWindowUpdate = 8
fromFrameTypeId FrameContinuation = 9
fromFrameTypeId (FrameUnknown x)  = x

-- |
--
-- >>> toFrameTypeId 0
-- FrameData
-- >>> toFrameTypeId 9
-- FrameContinuation
-- >>> toFrameTypeId 10
-- FrameUnknown 10
toFrameTypeId :: FrameType -> FrameTypeId
toFrameTypeId  0 = FrameData
toFrameTypeId  1 = FrameHeaders
toFrameTypeId  2 = FramePriority
toFrameTypeId  3 = FrameRSTStream
toFrameTypeId  4 = FrameSettings
toFrameTypeId  5 = FramePushPromise
toFrameTypeId  6 = FramePing
toFrameTypeId  7 = FrameGoAway
toFrameTypeId  8 = FrameWindowUpdate
toFrameTypeId  9 = FrameContinuation
toFrameTypeId  x = FrameUnknown x

----------------------------------------------------------------

-- | The maximum length of HTTP/2 payload.
--
-- >>> maxPayloadLength
-- 16384
maxPayloadLength :: Int
maxPayloadLength = 2^(14::Int)

----------------------------------------------------------------
-- Flags

type FrameFlags = Word8

-- |
-- >>> defaultFlags
-- 0
defaultFlags :: FrameFlags
defaultFlags = 0

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

-- |
-- >>> setEndStream 0
-- 1
setEndStream :: FrameFlags -> FrameFlags
setEndStream x = x `setBit` 0

-- |
-- >>> setAck 0
-- 1
setAck :: FrameFlags -> FrameFlags
setAck x = x `setBit` 0 -- fixme: is the spec intentional?

-- |
-- >>> setEndHeader 0
-- 4
setEndHeader :: FrameFlags -> FrameFlags
setEndHeader x = x `setBit` 2

-- |
-- >>> setPadded 0
-- 8
setPadded :: FrameFlags -> FrameFlags
setPadded x = x `setBit` 3

-- |
-- >>> setPriority 0
-- 32
setPriority :: FrameFlags -> FrameFlags
setPriority x = x `setBit` 5

----------------------------------------------------------------

newtype StreamIdentifier = StreamIdentifier Int deriving (Show, Read, Eq)
type StreamDependency    = StreamIdentifier
type LastStreamId        = StreamIdentifier
type PromisedStreamId    = StreamIdentifier

-- |
-- >>> toStreamIdentifier 0
-- StreamIdentifier 0
toStreamIdentifier :: Int -> StreamIdentifier
toStreamIdentifier n = StreamIdentifier (n `clearBit` 31)

-- |
-- >>> fromStreamIdentifier (toStreamIdentifier 0)
-- 0
fromStreamIdentifier :: StreamIdentifier -> Int
fromStreamIdentifier (StreamIdentifier n) = n

-- |
-- >>> isControl $ toStreamIdentifier 0
-- True
-- >>> isControl $ toStreamIdentifier 1
-- False
isControl :: StreamIdentifier -> Bool
isControl (StreamIdentifier 0) = True
isControl _                    = False

-- |
-- >>> isRequest $ toStreamIdentifier 0
-- False
-- >>> isRequest $ toStreamIdentifier 1
-- True
isRequest :: StreamIdentifier -> Bool
isRequest (StreamIdentifier n) = odd n

-- |
-- >>> isResponse $ toStreamIdentifier 0
-- False
-- >>> isResponse $ toStreamIdentifier 2
-- True
isResponse :: StreamIdentifier -> Bool
isResponse (StreamIdentifier 0) = False
isResponse (StreamIdentifier n) = even n

testExclusive :: Int -> Bool
testExclusive n = n `testBit` 31

setExclusive :: Int -> Int
setExclusive n = n `setBit` 31

----------------------------------------------------------------

type WindowSizeIncrement = Word32
type HeaderBlockFragment = ByteString
type Padding = ByteString

----------------------------------------------------------------

-- | The data type for HTTP/2 frames.
data Frame = Frame
    { frameHeader  :: FrameHeader
    , framePayload :: FramePayload
    } deriving (Show, Read, Eq)

-- | The data type for HTTP/2 frame headers.
data FrameHeader = FrameHeader
    { payloadLength :: Int
    , flags         :: FrameFlags
    , streamId      :: StreamIdentifier
    } deriving (Show, Read, Eq)

-- | The data type for HTTP/2 frame payloads.
data FramePayload =
    DataFrame ByteString
  | HeadersFrame (Maybe Priority) HeaderBlockFragment
  | PriorityFrame Priority
  | RSTStreamFrame ErrorCodeId
  | SettingsFrame SettingsList
  | PushPromiseFrame PromisedStreamId HeaderBlockFragment
  | PingFrame ByteString
  | GoAwayFrame LastStreamId ErrorCodeId ByteString
  | WindowUpdateFrame WindowSizeIncrement
  | ContinuationFrame HeaderBlockFragment
  | UnknownFrame FrameType ByteString
  deriving (Show, Read, Eq)

----------------------------------------------------------------

-- | Getting 'FrameType' from 'FramePayload'.
--
-- >>> framePayloadToFrameTypeId (DataFrame "body")
-- FrameData
framePayloadToFrameTypeId :: FramePayload -> FrameTypeId
framePayloadToFrameTypeId (DataFrame _)          = FrameData
framePayloadToFrameTypeId (HeadersFrame _ _)     = FrameHeaders
framePayloadToFrameTypeId (PriorityFrame _)      = FramePriority
framePayloadToFrameTypeId (RSTStreamFrame _)     = FrameRSTStream
framePayloadToFrameTypeId (SettingsFrame _)      = FrameSettings
framePayloadToFrameTypeId (PushPromiseFrame _ _) = FramePushPromise
framePayloadToFrameTypeId (PingFrame _)          = FramePing
framePayloadToFrameTypeId (GoAwayFrame _ _ _)    = FrameGoAway
framePayloadToFrameTypeId (WindowUpdateFrame _)  = FrameWindowUpdate
framePayloadToFrameTypeId (ContinuationFrame _)  = FrameContinuation
framePayloadToFrameTypeId (UnknownFrame w8 _)    = FrameUnknown w8
