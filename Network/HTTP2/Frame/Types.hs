{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Network.HTTP2.Frame.Types where

import Data.Ix
import Text.Read
import qualified Text.Read.Lex as L

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

-- | The type for error code. See <https://www.rfc-editor.org/rfc/rfc9113#ErrorCodes>.

pattern NoError            :: ErrorCode
pattern NoError             = ErrorCode 0x0

pattern ProtocolError      :: ErrorCode
pattern ProtocolError       = ErrorCode 0x1

pattern InternalError      :: ErrorCode
pattern InternalError       = ErrorCode 0x2

pattern FlowControlError   :: ErrorCode
pattern FlowControlError    = ErrorCode 0x3

pattern SettingsTimeout    :: ErrorCode
pattern SettingsTimeout     = ErrorCode 0x4

pattern StreamClosed       :: ErrorCode
pattern StreamClosed        = ErrorCode 0x5

pattern FrameSizeError     :: ErrorCode
pattern FrameSizeError      = ErrorCode 0x6

pattern RefusedStream      :: ErrorCode
pattern RefusedStream       = ErrorCode 0x7

pattern Cancel             :: ErrorCode
pattern Cancel              = ErrorCode 0x8

pattern CompressionError   :: ErrorCode
pattern CompressionError    = ErrorCode 0x9

pattern ConnectError       :: ErrorCode
pattern ConnectError        = ErrorCode 0xa

pattern EnhanceYourCalm    :: ErrorCode
pattern EnhanceYourCalm     = ErrorCode 0xb

pattern InadequateSecurity :: ErrorCode
pattern InadequateSecurity  = ErrorCode 0xc

pattern HTTP11Required     :: ErrorCode
pattern HTTP11Required      = ErrorCode 0xd

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

-- | The type for SETTINGS key.
newtype SettingsKey = SettingsKey Word16 deriving (Eq, Ord)

fromSettingsKey :: SettingsKey -> Word16
fromSettingsKey (SettingsKey x) = x

toSettingsKey :: Word16 -> SettingsKey
toSettingsKey = SettingsKey

minSettingsKey :: SettingsKey
minSettingsKey = SettingsKey 1

maxSettingsKey :: SettingsKey
maxSettingsKey = SettingsKey 6

pattern SettingsHeaderTableSize      :: SettingsKey
pattern SettingsHeaderTableSize       = SettingsKey 1

pattern SettingsEnablePush           :: SettingsKey
pattern SettingsEnablePush            = SettingsKey 2

pattern SettingsMaxConcurrentStreams :: SettingsKey
pattern SettingsMaxConcurrentStreams  = SettingsKey 3

pattern SettingsInitialWindowSize    :: SettingsKey
pattern SettingsInitialWindowSize     = SettingsKey 4

pattern SettingsMaxFrameSize         :: SettingsKey
pattern SettingsMaxFrameSize          = SettingsKey 5 -- this means payload size

pattern SettingsMaxHeaderBlockSize   :: SettingsKey
pattern SettingsMaxHeaderBlockSize    = SettingsKey 6

instance Show SettingsKey where
    show SettingsHeaderTableSize      = "SettingsHeaderTableSize"
    show SettingsEnablePush           = "SettingsEnablePush"
    show SettingsMaxConcurrentStreams = "SettingsMaxConcurrentStreams"
    show SettingsInitialWindowSize    = "SettingsInitialWindowSize"
    show SettingsMaxFrameSize         = "SettingsMaxFrameSize"
    show SettingsMaxHeaderBlockSize   = "SettingsMaxHeaderBlockSize"
    show (SettingsKey x)              = "SettingsKey " ++ show x

instance Read SettingsKey where
    readListPrec = readListPrecDefault
    readPrec = do
        Ident idnt <- lexP
        readSK idnt
      where
        readSK "SettingsHeaderTableSize"      = return SettingsHeaderTableSize
        readSK "SettingsEnablePush"           = return SettingsEnablePush
        readSK "SettingsMaxConcurrentStreams" = return SettingsMaxConcurrentStreams
        readSK "SettingsInitialWindowSize"    = return SettingsInitialWindowSize
        readSK "SettingsMaxFrameSize"         = return SettingsMaxFrameSize
        readSK "SettingsMaxHeaderBlockSize"   = return SettingsMaxHeaderBlockSize
        readSK "SettingsKey"         = do
              Number ftyp <- lexP
              return $ SettingsKey $ fromIntegral $ fromJust $ L.numberToInteger ftyp
        readSK _                   = error "Read for SettingsKey"

-- | The type for raw SETTINGS value.
type SettingsValue = Int -- Word32

-- | Association list of SETTINGS.
type SettingsList = [(SettingsKey,SettingsValue)]

----------------------------------------------------------------

-- | Cooked version of settings. This is suitable to be stored in a HTTP/2 context.
data Settings = Settings {
    headerTableSize :: Int
  , enablePush :: Bool
  , maxConcurrentStreams :: Maybe Int
  , initialWindowSize :: WindowSize
  , maxFrameSize :: Int
  , maxHeaderListSize :: Maybe Int
  } deriving (Show)

-- | The default settings.
--
-- >>> defaultSettings
-- Settings {headerTableSize = 4096, enablePush = True, maxConcurrentStreams = Nothing, initialWindowSize = WindowSize 65535, maxFrameSize = 16384, maxHeaderListSize = Nothing}
defaultSettings :: Settings
defaultSettings = Settings {
    headerTableSize = 4096 -- defaultDynamicTableSize
  , enablePush = True
  , maxConcurrentStreams = Nothing
  , initialWindowSize = defaultWindowSize
  , maxFrameSize = defaultPayloadLength
  , maxHeaderListSize = Nothing
  }

-- | Updating settings.
--
-- >>> updateSettings defaultSettings [(SettingsEnablePush,0),(SettingsMaxHeaderBlockSize,200)]
-- Settings {headerTableSize = 4096, enablePush = False, maxConcurrentStreams = Nothing, initialWindowSize = WindowSize 65535, maxFrameSize = 16384, maxHeaderListSize = Just 200}
updateSettings :: Settings -> SettingsList -> Settings
updateSettings settings kvs = foldl' update settings kvs
  where
    update def (SettingsHeaderTableSize,x)      = def { headerTableSize = x }
    -- fixme: x should be 0 or 1
    update def (SettingsEnablePush,x)           = def { enablePush = x > 0 }
    update def (SettingsMaxConcurrentStreams,x) = def { maxConcurrentStreams = Just x }
    update def (SettingsInitialWindowSize,x)    = def { initialWindowSize = WindowSize (fromIntegral x) }
    update def (SettingsMaxFrameSize,x)         = def { maxFrameSize = x }
    update def (SettingsMaxHeaderBlockSize,x)   = def { maxHeaderListSize = Just x }
    update def _                                = def

-- | The type for window size.
newtype WindowSize = WindowSize Word deriving (Eq, Ord, Show, Read, Num)
-- Even on 32 bit architecture, 'Word' is safe since max window size
-- is 2^31 - 1.

fromWindowSize :: WindowSize -> Int
fromWindowSize (WindowSize w) = fromIntegral w

toWindowSize :: Int -> WindowSize
toWindowSize w = WindowSize $ fromIntegral w

-- | The default initial window size.
--
-- >>> defaultWindowSize
-- WindowSize 65535
defaultWindowSize :: WindowSize
defaultWindowSize = WindowSize 65535

-- | The maximum window size.
--
-- >>> maxWindowSize
-- WindowSize 2147483647
maxWindowSize :: WindowSize
maxWindowSize = WindowSize 2147483647

-- | Checking if a window size exceeds the maximum window size.
--
-- >>> isWindowOverflow 10
-- False
-- >>> isWindowOverflow maxWindowSize
-- False
-- >>> isWindowOverflow (maxWindowSize + 1)
-- True
isWindowOverflow :: WindowSize -> Bool
isWindowOverflow (WindowSize w) = testBit w 31


-- | Default concurrency.
--
-- >>> recommendedConcurrency
-- 100
recommendedConcurrency :: Int
recommendedConcurrency = 100

----------------------------------------------------------------

-- | The type for weight in priority. Its values are from 1 to 256.
--   Deprecated in RFC 9113.
type Weight = Int

-- | Type for stream priority. Deprecated in RFC 9113 but provided for 'FrameHeaders'.
data Priority = Priority {
    exclusive :: Bool
  , streamDependency :: StreamId
  , weight :: Weight
  } deriving (Show, Read, Eq)

----------------------------------------------------------------

-- | The type for raw frame type.
newtype FrameType = FrameType Word8 deriving (Eq, Ord, Ix)

-- | Converting 'FrameType' to 'Word8'.
--
-- >>> fromFrameType FrameData
-- 0
-- >>> fromFrameType FrameContinuation
-- 9
fromFrameType :: FrameType -> Word8
fromFrameType (FrameType x) = x

toFrameType :: Word8 -> FrameType
toFrameType = FrameType

minFrameType :: FrameType
minFrameType = FrameType 0

maxFrameType :: FrameType
maxFrameType = FrameType 9

pattern FrameData         :: FrameType
pattern FrameData          = FrameType 0

pattern FrameHeaders      :: FrameType
pattern FrameHeaders       = FrameType 1

pattern FramePriority     :: FrameType
pattern FramePriority      = FrameType 2

pattern FrameRSTStream    :: FrameType
pattern FrameRSTStream     = FrameType 3

pattern FrameSettings     :: FrameType
pattern FrameSettings      = FrameType 4

pattern FramePushPromise  :: FrameType
pattern FramePushPromise   = FrameType 5

pattern FramePing         :: FrameType
pattern FramePing          = FrameType 6

pattern FrameGoAway       :: FrameType
pattern FrameGoAway        = FrameType 7

pattern FrameWindowUpdate :: FrameType
pattern FrameWindowUpdate  = FrameType 8

pattern FrameContinuation :: FrameType
pattern FrameContinuation  = FrameType 9

instance Show FrameType where
    show (FrameType 0) = "FrameData"
    show (FrameType 1) = "FrameHeaders"
    show (FrameType 2) = "FramePriority"
    show (FrameType 3) = "FrameRSTStream"
    show (FrameType 4) = "FrameSettings"
    show (FrameType 5) = "FramePushPromise"
    show (FrameType 6) = "FramePing"
    show (FrameType 7) = "FrameGoAway"
    show (FrameType 8) = "FrameWindowUpdate"
    show (FrameType 9) = "FrameContinuation"
    show (FrameType x) = "FrameType " ++ show x

instance Read FrameType where
    readListPrec = readListPrecDefault
    readPrec = do
        Ident idnt <- lexP
        readFT idnt
      where
        readFT "FrameData"         = return FrameData
        readFT "FrameHeaders"      = return FrameHeaders
        readFT "FramePriority"     = return FramePriority
        readFT "FrameRSTStream"    = return FrameRSTStream
        readFT "FrameSettings"     = return FrameSettings
        readFT "FramePushPromise"  = return FramePushPromise
        readFT "FramePing"         = return FramePing
        readFT "FrameGoAway"       = return FrameGoAway
        readFT "FrameWindowUpdate" = return FrameWindowUpdate
        readFT "FrameContinuation" = return FrameContinuation
        readFT "FrameType"         = do
              Number ftyp <- lexP
              return $ FrameType $ fromIntegral $ fromJust $ L.numberToInteger ftyp
        readFT _                   = error "Read for FrameType"

----------------------------------------------------------------

-- | The maximum payload length of HTTP/2 payload.
--
-- >>> maxPayloadLength
-- 16777215
maxPayloadLength :: Int
maxPayloadLength = 2^(24::Int) - 1

-- | The default payload length of HTTP/2 payload.
--
-- >>> defaultPayloadLength
-- 16384
defaultPayloadLength :: Int
defaultPayloadLength = 2^(14::Int)

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
  | GoAwayFrame {- the last -}StreamId ErrorCode ByteString
  | WindowUpdateFrame WindowSize
  | ContinuationFrame HeaderBlockFragment
  | UnknownFrame FrameType ByteString
  deriving (Show, Read, Eq)

----------------------------------------------------------------

-- | Getting 'FrameType' from 'FramePayload'.
--
-- >>> framePayloadToFrameType (DataFrame "body")
-- FrameData
framePayloadToFrameType :: FramePayload -> FrameType
framePayloadToFrameType DataFrame{}          = FrameData
framePayloadToFrameType HeadersFrame{}       = FrameHeaders
framePayloadToFrameType PriorityFrame{}      = FramePriority
framePayloadToFrameType RSTStreamFrame{}     = FrameRSTStream
framePayloadToFrameType SettingsFrame{}      = FrameSettings
framePayloadToFrameType PushPromiseFrame{}   = FramePushPromise
framePayloadToFrameType PingFrame{}          = FramePing
framePayloadToFrameType GoAwayFrame{}        = FrameGoAway
framePayloadToFrameType WindowUpdateFrame{}  = FrameWindowUpdate
framePayloadToFrameType ContinuationFrame{}  = FrameContinuation
framePayloadToFrameType (UnknownFrame ft _)  = ft

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

----------------------------------------------------------------
-- Deprecated

type ErrorCodeId   = ErrorCode
type SettingsKeyId = SettingsKey
type FrameTypeId   = FrameType
{- DEPRECATED ErrorCodeId   "Use ErrorCode instead" -}
{- DEPRECATED SettingsKeyId "Use SettingsKey instead" -}
{- DEPRECATED FrameTypeId   "Use FrameType instead" -}
