{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Network.HTTP2.Frame.Types where

import Data.Ix
import Network.Control (WindowSize)
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

{- FOURMOLU_DISABLE -}
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
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
instance Show ErrorCode where
    show NoError            = "NoError"
    show ProtocolError      = "ProtocolError"
    show InternalError      = "InternalError"
    show FlowControlError   = "FlowControlError"
    show SettingsTimeout    = "SettingsTimeout"
    show StreamClosed       = "StreamClosed"
    show FrameSizeError     = "FrameSizeError"
    show RefusedStream      = "RefusedStream"
    show Cancel             = "Cancel"
    show CompressionError   = "CompressionError"
    show ConnectError       = "ConnectError"
    show EnhanceYourCalm    = "EnhanceYourCalm"
    show InadequateSecurity = "InadequateSecurity"
    show HTTP11Required     = "HTTP11Required"
    show (ErrorCode x)      = "ErrorCode " ++ show x
{- FOURMOLU_ENABLE -}

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

{- FOURMOLU_DISABLE -}
pattern SettingsTokenHeaderTableSize :: SettingsKey
pattern SettingsTokenHeaderTableSize  = SettingsKey 1

pattern SettingsEnablePush           :: SettingsKey
pattern SettingsEnablePush            = SettingsKey 2

pattern SettingsMaxConcurrentStreams :: SettingsKey
pattern SettingsMaxConcurrentStreams  = SettingsKey 3

pattern SettingsInitialWindowSize    :: SettingsKey
pattern SettingsInitialWindowSize     = SettingsKey 4

pattern SettingsMaxFrameSize         :: SettingsKey
pattern SettingsMaxFrameSize          = SettingsKey 5 -- this means payload size

pattern SettingsMaxHeaderListSize    :: SettingsKey
pattern SettingsMaxHeaderListSize     = SettingsKey 6
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
instance Show SettingsKey where
    show SettingsTokenHeaderTableSize = "SettingsTokenHeaderTableSize"
    show SettingsEnablePush           = "SettingsEnablePush"
    show SettingsMaxConcurrentStreams = "SettingsMaxConcurrentStreams"
    show SettingsInitialWindowSize    = "SettingsInitialWindowSize"
    show SettingsMaxFrameSize         = "SettingsMaxFrameSize"
    show SettingsMaxHeaderListSize    = "SettingsMaxHeaderListSize"
    show (SettingsKey x)              = "SettingsKey " ++ show x
{- FOURMOLU_ENABLE -}

instance Read SettingsKey where
    readListPrec = readListPrecDefault
    readPrec = do
        Ident idnt <- lexP
        readSK idnt
      where
        readSK "SettingsTokenHeaderTableSize" = return SettingsTokenHeaderTableSize
        readSK "SettingsEnablePush" = return SettingsEnablePush
        readSK "SettingsMaxConcurrentStreams" = return SettingsMaxConcurrentStreams
        readSK "SettingsInitialWindowSize" = return SettingsInitialWindowSize
        readSK "SettingsMaxFrameSize" = return SettingsMaxFrameSize
        readSK "SettingsMaxHeaderListSize" = return SettingsMaxHeaderListSize
        readSK "SettingsKey" = do
            Number ftyp <- lexP
            return $ SettingsKey $ fromIntegral $ fromJust $ L.numberToInteger ftyp
        readSK _ = error "Read for SettingsKey"

-- | The type for raw SETTINGS value.
type SettingsValue = Int -- Word32

-- | Association list of SETTINGS.
type SettingsList = [(SettingsKey, SettingsValue)]

-- | The default initial window size.
--
-- >>> defaultWindowSize
-- 65535
defaultWindowSize :: WindowSize
defaultWindowSize = 65535

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
isWindowOverflow w = w > maxWindowSize

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
data Priority = Priority
    { exclusive :: Bool
    , streamDependency :: StreamId
    , weight :: Weight
    }
    deriving (Show, Read, Eq)

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

{- FOURMOLU_DISABLE -}
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
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
instance Show FrameType where
    show FrameData         = "FrameData"
    show FrameHeaders      = "FrameHeaders"
    show FramePriority     = "FramePriority"
    show FrameRSTStream    = "FrameRSTStream"
    show FrameSettings     = "FrameSettings"
    show FramePushPromise  = "FramePushPromise"
    show FramePing         = "FramePing"
    show FrameGoAway       = "FrameGoAway"
    show FrameWindowUpdate = "FrameWindowUpdate"
    show FrameContinuation = "FrameContinuation"
    show (FrameType x)     = "FrameType " ++ show x
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
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
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

-- | The maximum payload length of HTTP/2 payload.
--
-- >>> maxPayloadLength
-- 16777215
maxPayloadLength :: Int
maxPayloadLength = 2 ^ (24 :: Int) - 1

-- | The default payload length of HTTP/2 payload.
--
-- >>> defaultPayloadLength
-- 16384
defaultPayloadLength :: Int
defaultPayloadLength = 2 ^ (14 :: Int)

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
    { frameHeader :: FrameHeader
    , framePayload :: FramePayload
    }
    deriving (Show, Read, Eq)

-- | The data type for HTTP/2 frame headers.
data FrameHeader = FrameHeader
    { payloadLength :: Int
    , flags :: FrameFlags
    , streamId :: StreamId
    }
    deriving (Show, Read, Eq)

-- | The data type for HTTP/2 frame payloads.
data FramePayload
    = DataFrame ByteString
    | HeadersFrame (Maybe Priority) HeaderBlockFragment
    | PriorityFrame Priority
    | RSTStreamFrame ErrorCode
    | SettingsFrame SettingsList
    | PushPromiseFrame StreamId HeaderBlockFragment
    | PingFrame ByteString
    | GoAwayFrame {- the last -} StreamId ErrorCode ByteString
    | WindowUpdateFrame WindowSize
    | ContinuationFrame HeaderBlockFragment
    | UnknownFrame FrameType ByteString
    deriving (Show, Read, Eq)

----------------------------------------------------------------

-- | Getting 'FrameType' from 'FramePayload'.
--
-- >>> framePayloadToFrameType (DataFrame "body")
-- FrameData
{- FOURMOLU_DISABLE -}
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
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

-- | Checking if padding is defined in this frame type.
--
-- >>> isPaddingDefined $ DataFrame ""
-- True
-- >>> isPaddingDefined $ PingFrame ""
-- False
{- FOURMOLU_DISABLE -}
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
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------
-- Deprecated

{- FOURMOLU_DISABLE -}
type ErrorCodeId   = ErrorCode
type SettingsKeyId = SettingsKey
type FrameTypeId   = FrameType
{- FOURMOLU_ENABLE -}
{- DEPRECATED ErrorCodeId   "Use ErrorCode instead" -}
{- DEPRECATED SettingsKeyId "Use SettingsKey instead" -}
{- DEPRECATED FrameTypeId   "Use FrameType instead" -}
