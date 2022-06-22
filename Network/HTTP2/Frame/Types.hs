{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Network.HTTP2.Frame.Types (
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
  -- * Error code
  , ErrorCode(ErrorCode,NoError,ProtocolError,InternalError,FlowControlError,SettingsTimeout,StreamClosed,FrameSizeError,RefusedStream,Cancel,CompressionError,ConnectError,EnhanceYourCalm,InadequateSecurity,HTTP11Required)
  , fromErrorCode
  , toErrorCode
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
  , isPaddingDefined
  -- * Stream identifier
  , StreamId
  , isControl
  , isClientInitiated
  , isServerInitiated
  , isRequest
  , isResponse
  , testExclusive
  , setExclusive
  , clearExclusive
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
  -- * Window
  , WindowSize
  , defaultInitialWindowSize
  , maxWindowSize
  , isWindowOverflow
  -- * Misc
  , recommendedConcurrency
  -- * Types
  , HeaderBlockFragment
  , Weight
  , defaultWeight
  , Priority(..)
  , defaultPriority
  , highestPriority
  , Padding
  ) where

import qualified Control.Exception as E
import Data.Typeable

import Imports

----------------------------------------------------------------

-- | The length of HTTP/2 frame header.
--
-- >>> frameHeaderLength
-- 9
frameHeaderLength :: Int
frameHeaderLength = 9

----------------------------------------------------------------

-- | The type for raw error code.
newtype ErrorCode = ErrorCode Word32 deriving (Eq, Ord, Read)

fromErrorCode :: ErrorCode -> Word32
fromErrorCode (ErrorCode w) = w

toErrorCode :: Word32 -> ErrorCode
toErrorCode = ErrorCode

-- | The type for error code. See <https://tools.ietf.org/html/rfc7540#section-7>.

pattern NoError :: ErrorCode
pattern NoError            = ErrorCode 0x0

pattern ProtocolError :: ErrorCode
pattern ProtocolError      = ErrorCode 0x1

pattern InternalError :: ErrorCode
pattern InternalError      = ErrorCode 0x2

pattern FlowControlError :: ErrorCode
pattern FlowControlError   = ErrorCode 0x3

pattern SettingsTimeout :: ErrorCode
pattern SettingsTimeout    = ErrorCode 0x4

pattern StreamClosed :: ErrorCode
pattern StreamClosed       = ErrorCode 0x5

pattern FrameSizeError :: ErrorCode
pattern FrameSizeError     = ErrorCode 0x6

pattern RefusedStream :: ErrorCode
pattern RefusedStream      = ErrorCode 0x7

pattern Cancel :: ErrorCode
pattern Cancel             = ErrorCode 0x8

pattern CompressionError :: ErrorCode
pattern CompressionError   = ErrorCode 0x9

pattern ConnectError :: ErrorCode
pattern ConnectError       = ErrorCode 0xa

pattern EnhanceYourCalm :: ErrorCode
pattern EnhanceYourCalm    = ErrorCode 0xb

pattern InadequateSecurity :: ErrorCode
pattern InadequateSecurity = ErrorCode 0xc

pattern HTTP11Required :: ErrorCode
pattern HTTP11Required     = ErrorCode 0xd

instance Show ErrorCode where
    show (ErrorCode 0x0) = "NoError"
    show (ErrorCode 0x1) = "ProtocolError"
    show (ErrorCode 0x2) = "InternalError"
    show (ErrorCode 0x3) = "FlowControlError"
    show (ErrorCode 0x4) = "SettingsTimeout"
    show (ErrorCode 0x5) = "StreamClosed"
    show (ErrorCode 0x6) = "FrameSizeError"
    show (ErrorCode 0x7) = "RefusedStream"
    show (ErrorCode 0x8) = "Cancel"
    show (ErrorCode 0x9) = "CompressionError"
    show (ErrorCode 0xa) = "ConnectError"
    show (ErrorCode 0xb) = "EnhanceYourCalm"
    show (ErrorCode 0xc) = "InadequateSecurity"
    show (ErrorCode 0xd) = "HTTP11Required"
    show (ErrorCode   x) = "ErrorCode " ++ show x

----------------------------------------------------------------

type ReasonPhrase = ShortByteString

-- | The connection error or the stream error.
data HTTP2Error =
    ConnectionIsClosed -- NoError
  | ConnectionErrorIsReceived ErrorCode ReasonPhrase
  | ConnectionErrorIsSent     ErrorCode ReasonPhrase
  | StreamErrorIsReceived     ErrorCode StreamId
  | StreamErrorIsSent         ErrorCode StreamId
  | BadThingHappen E.SomeException
  deriving (Show, Typeable)

instance E.Exception HTTP2Error

----------------------------------------------------------------

-- | The type for SETTINGS key.
data SettingsKeyId = SettingsHeaderTableSize
                   | SettingsEnablePush
                   | SettingsMaxConcurrentStreams
                   | SettingsInitialWindowSize
                   | SettingsMaxFrameSize -- this means payload size
                   | SettingsMaxHeaderBlockSize
                   deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | The type for raw SETTINGS value.
type SettingsValue = Int -- Word32

-- | Converting 'SettingsKeyId' to raw value.
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

-- | Converting raw value to 'SettingsKeyId'.
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

-- | Association list of SETTINGS.
type SettingsList = [(SettingsKeyId,SettingsValue)]

-- | Checking 'SettingsList' and reporting an error if any.
--
-- >>> checkSettingsList [(SettingsEnablePush,2)]
-- Just (ConnectionErrorIsSent ProtocolError "enable push must be 0 or 1")
checkSettingsList :: SettingsList -> Maybe HTTP2Error
checkSettingsList settings = case mapMaybe checkSettingsValue settings of
    []    -> Nothing
    (x:_) -> Just x

checkSettingsValue :: (SettingsKeyId,SettingsValue) -> Maybe HTTP2Error
checkSettingsValue (SettingsEnablePush,v)
  | v /= 0 && v /= 1 = Just $ ConnectionErrorIsSent ProtocolError "enable push must be 0 or 1"
checkSettingsValue (SettingsInitialWindowSize,v)
  | v > 2147483647   = Just $ ConnectionErrorIsSent FlowControlError "Window size must be less than or equal to 65535"
checkSettingsValue (SettingsMaxFrameSize,v)
  | v < 16384 || v > 16777215 = Just $ ConnectionErrorIsSent ProtocolError "Max frame size must be in between 16384 and 16777215"
checkSettingsValue _ = Nothing

----------------------------------------------------------------

-- | Cooked version of settings. This is suitable to be stored in a HTTP/2 context.
data Settings = Settings {
    headerTableSize :: Int
  , enablePush :: Bool
  , maxConcurrentStreams :: Maybe Int
  , initialWindowSize :: WindowSize
  , maxFrameSize :: Int
  , maxHeaderBlockSize :: Maybe Int
  } deriving (Show)

-- | The default settings.
--
-- >>> defaultSettings
-- Settings {headerTableSize = 4096, enablePush = True, maxConcurrentStreams = Nothing, initialWindowSize = 65535, maxFrameSize = 16384, maxHeaderBlockSize = Nothing}
defaultSettings :: Settings
defaultSettings = Settings {
    headerTableSize = 4096
  , enablePush = True
  , maxConcurrentStreams = Nothing
  , initialWindowSize = defaultInitialWindowSize
  , maxFrameSize = 16384
  , maxHeaderBlockSize = Nothing
  }

-- | Updating settings.
--
-- >>> updateSettings defaultSettings [(SettingsEnablePush,0),(SettingsMaxHeaderBlockSize,200)]
-- Settings {headerTableSize = 4096, enablePush = False, maxConcurrentStreams = Nothing, initialWindowSize = 65535, maxFrameSize = 16384, maxHeaderBlockSize = Just 200}
updateSettings :: Settings -> SettingsList -> Settings
updateSettings settings kvs = foldl' update settings kvs
  where
    update def (SettingsHeaderTableSize,x)      = def { headerTableSize = x }
    -- fixme: x should be 0 or 1
    update def (SettingsEnablePush,x)           = def { enablePush = x > 0 }
    update def (SettingsMaxConcurrentStreams,x) = def { maxConcurrentStreams = Just x }
    update def (SettingsInitialWindowSize,x)    = def { initialWindowSize = x }
    update def (SettingsMaxFrameSize,x)         = def { maxFrameSize = x }
    update def (SettingsMaxHeaderBlockSize,x)   = def { maxHeaderBlockSize = Just x }

-- | The type for window size.
type WindowSize = Int

-- | The default initial window size.
--
-- >>> defaultInitialWindowSize
-- 65535
defaultInitialWindowSize :: WindowSize
defaultInitialWindowSize = 65535

-- | The maximum window size.
--
-- >>> maxWindowSize
-- 2147483647
maxWindowSize :: WindowSize
maxWindowSize = 2147483647

-- | Checking if a window size exceeds the maximum window size.
--
-- >>> isWindowOverflow 10
-- False
-- >>> isWindowOverflow maxWindowSize
-- False
-- >>> isWindowOverflow (maxWindowSize + 1)
-- True
isWindowOverflow :: WindowSize -> Bool
isWindowOverflow w = testBit w 31


-- | Default concurrency.
--
-- >>> recommendedConcurrency
-- 100
recommendedConcurrency :: Int
recommendedConcurrency = 100

----------------------------------------------------------------

-- | The type for weight in priority. Its values are from 1 to 256.
type Weight = Int

-- | Default weight.
--
-- >>> defaultWeight
-- 16
defaultWeight :: Weight
defaultWeight = 16
{-# DEPRECATED defaultWeight "Don't use this" #-}

-- | Type for stream priority
data Priority = Priority {
    exclusive :: Bool
  , streamDependency :: StreamId
  , weight :: Weight
  } deriving (Show, Read, Eq)

-- | Default priority which depends on stream 0.
--
-- >>> defaultPriority
-- Priority {exclusive = False, streamDependency = 0, weight = 16}
defaultPriority :: Priority
defaultPriority = Priority False 0 defaultWeight

-- | Highest priority which depends on stream 0.
--
-- >>> highestPriority
-- Priority {exclusive = False, streamDependency = 0, weight = 256}
highestPriority :: Priority
highestPriority = Priority False 0 256

----------------------------------------------------------------

-- | The type for raw frame type.
type FrameType = Word8

minFrameType :: FrameType
minFrameType = 0

maxFrameType :: FrameType
maxFrameType = 9

-- | The type for frame type.
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

-- | Converting 'FrameTypeId' to 'FrameType'.
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

-- | Converting 'FrameType' to 'FrameTypeId'.
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

-- | The type for flags.
type FrameFlags = Word8

-- | The initial value of flags. No flags are set.
--
-- >>> defaultFlags
-- 0
defaultFlags :: FrameFlags
defaultFlags = 0

-- | Checking if the END_STREAM flag is set.
-- >>> testEndStream 0x1
-- True
testEndStream :: FrameFlags -> Bool
testEndStream x = x `testBit` 0

-- | Checking if the ACK flag is set.
-- >>> testAck 0x1
-- True
testAck :: FrameFlags -> Bool
testAck x = x `testBit` 0 -- fixme: is the spec intentional?

-- | Checking if the END_HEADERS flag is set.
--
-- >>> testEndHeader 0x4
-- True
testEndHeader :: FrameFlags -> Bool
testEndHeader x = x `testBit` 2

-- | Checking if the PADDED flag is set.
--
-- >>> testPadded 0x8
-- True
testPadded :: FrameFlags -> Bool
testPadded x = x `testBit` 3

-- | Checking if the PRIORITY flag is set.
--
-- >>> testPriority 0x20
-- True
testPriority :: FrameFlags -> Bool
testPriority x = x `testBit` 5

-- | Setting the END_STREAM flag.
--
-- >>> setEndStream 0
-- 1
setEndStream :: FrameFlags -> FrameFlags
setEndStream x = x `setBit` 0

-- | Setting the ACK flag.
--
-- >>> setAck 0
-- 1
setAck :: FrameFlags -> FrameFlags
setAck x = x `setBit` 0 -- fixme: is the spec intentional?

-- | Setting the END_HEADERS flag.
--
-- >>> setEndHeader 0
-- 4
setEndHeader :: FrameFlags -> FrameFlags
setEndHeader x = x `setBit` 2

-- | Setting the PADDED flag.
--
-- >>> setPadded 0
-- 8
setPadded :: FrameFlags -> FrameFlags
setPadded x = x `setBit` 3

-- | Setting the PRIORITY flag.
--
-- >>> setPriority 0
-- 32
setPriority :: FrameFlags -> FrameFlags
setPriority x = x `setBit` 5

----------------------------------------------------------------

-- | The type for stream identifier
type StreamId = Int

-- | Checking if the stream identifier for control.
--
-- >>> isControl 0
-- True
-- >>> isControl 1
-- False
isControl :: StreamId -> Bool
isControl 0 = True
isControl _ = False

-- | Checking if the stream identifier is from a client.
--
-- >>> isClientInitiated 0
-- False
-- >>> isClientInitiated 1
-- True
isClientInitiated :: StreamId -> Bool
isClientInitiated = odd

-- | Checking if the stream identifier is from a server.
--
-- >>> isServerInitiated 0
-- False
-- >>> isServerInitiated 2
-- True
isServerInitiated :: StreamId -> Bool
isServerInitiated 0 = False
isServerInitiated n = even n

-- | Checking if the stream identifier for request.
--
-- >>> isRequest 0
-- False
-- >>> isRequest 1
-- True
isRequest :: StreamId -> Bool
isRequest = odd
{-# DEPRECATED isRequest "Use isClientInitiated instead" #-}

-- | Checking if the stream identifier for response.
--
-- >>> isResponse 0
-- False
-- >>> isResponse 2
-- True
isResponse :: StreamId -> Bool
isResponse 0 = False
isResponse n = even n
{-# DEPRECATED isResponse "Use isServerInitiated instead" #-}

-- | Checking if the exclusive flag is set.
testExclusive :: StreamId -> Bool
testExclusive n = n `testBit` 31

-- | Setting the exclusive flag.
setExclusive :: StreamId -> StreamId
setExclusive n = n `setBit` 31

-- | Clearing the exclusive flag.
clearExclusive :: StreamId -> StreamId
clearExclusive n = n `clearBit` 31

----------------------------------------------------------------

-- | The type for fragments of a header encoded with HPACK.
type HeaderBlockFragment = ByteString

-- | The type for padding in payloads.
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
    , streamId      :: StreamId
    } deriving (Show, Read, Eq)

-- | The data type for HTTP/2 frame payloads.
data FramePayload =
    DataFrame ByteString
  | HeadersFrame (Maybe Priority) HeaderBlockFragment
  | PriorityFrame Priority
  | RSTStreamFrame ErrorCode
  | SettingsFrame SettingsList
  | PushPromiseFrame StreamId HeaderBlockFragment
  | PingFrame ByteString
  | GoAwayFrame StreamId ErrorCode ByteString
  | WindowUpdateFrame WindowSize
  | ContinuationFrame HeaderBlockFragment
  | UnknownFrame FrameType ByteString
  deriving (Show, Read, Eq)

----------------------------------------------------------------

-- | Getting 'FrameType' from 'FramePayload'.
--
-- >>> framePayloadToFrameTypeId (DataFrame "body")
-- FrameData
framePayloadToFrameTypeId :: FramePayload -> FrameTypeId
framePayloadToFrameTypeId DataFrame{}          = FrameData
framePayloadToFrameTypeId HeadersFrame{}       = FrameHeaders
framePayloadToFrameTypeId PriorityFrame{}      = FramePriority
framePayloadToFrameTypeId RSTStreamFrame{}     = FrameRSTStream
framePayloadToFrameTypeId SettingsFrame{}      = FrameSettings
framePayloadToFrameTypeId PushPromiseFrame{}   = FramePushPromise
framePayloadToFrameTypeId PingFrame{}          = FramePing
framePayloadToFrameTypeId GoAwayFrame{}        = FrameGoAway
framePayloadToFrameTypeId WindowUpdateFrame{}  = FrameWindowUpdate
framePayloadToFrameTypeId ContinuationFrame{}  = FrameContinuation
framePayloadToFrameTypeId (UnknownFrame w8 _)  = FrameUnknown w8

----------------------------------------------------------------

-- | Checking if padding is defined in this frame type.
--
-- >>> isPaddingDefined $ DataFrame ""
-- True
-- >>> isPaddingDefined $ PingFrame ""
-- False
isPaddingDefined :: FramePayload -> Bool
isPaddingDefined DataFrame{}         = True
isPaddingDefined HeadersFrame{}      = True
isPaddingDefined PriorityFrame{}     = False
isPaddingDefined RSTStreamFrame{}    = False
isPaddingDefined SettingsFrame{}     = False
isPaddingDefined PushPromiseFrame{}  = True
isPaddingDefined PingFrame{}         = False
isPaddingDefined GoAwayFrame{}       = False
isPaddingDefined WindowUpdateFrame{} = False
isPaddingDefined ContinuationFrame{} = False
isPaddingDefined UnknownFrame{}      = False
