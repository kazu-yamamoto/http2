module Network.HTTP2.Types where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Word (Word8, Word16, Word32)
import qualified Data.Map as Map -- FIXME

----------------------------------------------------------------

-- Basic odd length HTTP/2 ints
type Int24 = Int32
type Int31 = Int32

-- Custom type aliases for HTTP/2 parts
type RSTStreamErrorCode  = Word32
type HeaderBlockFragment = ByteString
type StreamDependency    = Int31
type LastStreamId        = Int31
type PromisedStreamId    = Int31
type WindowSizeIncrement = Int31
type Exclusive           = Bool
type Weight              = Int

----------------------------------------------------------------

-- Valid SettingID's
data SettingID = SettingHeaderTableSize
               | SettingEnablePush
               | SettingMaxConcurrentStreams
               | SettingInitialWindowSize
               | SettingMaxFrameSize
               | SettingMaxHeaderBlockSize
               | SettingUnknown
               deriving (Show, Eq, Ord, Enum, Bounded)

settingIdToWord16 :: SettingID -> Word16
settingIdToWord16 SettingHeaderTableSize      = 0x1
settingIdToWord16 SettingEnablePush           = 0x2
settingIdToWord16 SettingMaxConcurrentStreams = 0x3
settingIdToWord16 SettingInitialWindowSize    = 0x4
settingIdToWord16 SettingMaxFrameSize         = 0x5
settingIdToWord16 SettingMaxHeaderBlockSize   = 0x6
settingIdToWord16 SettingUnknown              = maxBound

settingIdFromWord16 :: Word16 -> SettingID
settingIdFromWord16 0x1 = SettingHeaderTableSize
settingIdFromWord16 0x2 = SettingEnablePush
settingIdFromWord16 0x3 = SettingMaxConcurrentStreams
settingIdFromWord16 0x4 = SettingInitialWindowSize
settingIdFromWord16 0x5 = SettingMaxFrameSize
settingIdFromWord16 0x6 = SettingMaxHeaderBlockSize
settingIdFromWord16 _   = SettingUnknown

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
               | FrameUnknown
               deriving (Show, Eq, Ord, Enum, Bounded)

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
frameTypeToWord8 FrameUnknown      = maxBound

-- FIXME
frameTypeFromWord8 :: Word8 -> FrameType
frameTypeFromWord8 0x0 = FrameData
frameTypeFromWord8 0x1 = FrameHeaders
frameTypeFromWord8 0x2 = FramePriority
frameTypeFromWord8 0x3 = FrameRSTStream
frameTypeFromWord8 0x4 = FrameSettings
frameTypeFromWord8 0x5 = FramePushPromise
frameTypeFromWord8 0x6 = FramePing
frameTypeFromWord8 0x7 = FrameGoAway
frameTypeFromWord8 0x8 = FrameWindowUpdate
frameTypeFromWord8 0x9 = FrameContinuation
frameTypeFromWord8 _   = FrameUnknown

----------------------------------------------------------------

-- A complete frame header
data FrameHeader = FrameHeader
    { fhType     :: FrameType
    , fhFlags    :: Word8
    , fhLength   :: Int24
    , fhStreamId :: Word32
    } deriving (Show, Eq)

-- The raw frame is the header with the payload body, but not a parsed
-- full frame
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
           | WindowUpdateFrame WindowSizeIncrement
           | ContinuationFrame HeaderBlockFragment
           | UnknownFrame ByteString

-- Valid settings map
type SettingsMap = Map.Map SettingID Word32

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
               | UnknownError
               deriving (Show, Eq, Ord, Enum, Bounded)

errorCodeToWord32 :: ErrorCode -> Word32
errorCodeToWord32 NoError            = 0x0
errorCodeToWord32 ProtocolError      = 0x1
errorCodeToWord32 InternalError      = 0x2
errorCodeToWord32 FlowControlError   = 0x3
errorCodeToWord32 SettingsTimeout    = 0x4
errorCodeToWord32 StreamClosed       = 0x5
errorCodeToWord32 FrameSizeError     = 0x6
errorCodeToWord32 RefusedStream      = 0x7
errorCodeToWord32 Cancel             = 0x8
errorCodeToWord32 CompressionError   = 0x9
errorCodeToWord32 ConnectError       = 0xa
errorCodeToWord32 EnhanceYourCalm    = 0xb
errorCodeToWord32 InadequateSecurity = 0xc
errorCodeToWord32 UnknownError       = maxBound

errorCodeFromWord32 :: Word32 -> ErrorCode
errorCodeFromWord32 0x0 = NoError
errorCodeFromWord32 0x1 = ProtocolError
errorCodeFromWord32 0x2 = InternalError
errorCodeFromWord32 0x3 = FlowControlError
errorCodeFromWord32 0x4 = SettingsTimeout
errorCodeFromWord32 0x5 = StreamClosed
errorCodeFromWord32 0x6 = FrameSizeError
errorCodeFromWord32 0x7 = RefusedStream
errorCodeFromWord32 0x8 = Cancel
errorCodeFromWord32 0x9 = CompressionError
errorCodeFromWord32 0xa = ConnectError
errorCodeFromWord32 0xb = EnhanceYourCalm
errorCodeFromWord32 0xc = InadequateSecurity
errorCodeFromWord32 _   = UnknownError
