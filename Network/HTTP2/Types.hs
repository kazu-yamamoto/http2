module Network.HTTP2.Types where

import Data.Array (Array, Ix, listArray)
import Data.Array.ST (newArray, writeArray, runSTArray)
import Data.ByteString (ByteString)
import Data.Word (Word8, Word16, Word32)
import Control.Monad (forM_)
import Data.Bits (setBit, testBit, clearBit)

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
fromErrorCodeId (UnknownErrorCode w) = w
fromErrorCodeId _                    = 255 -- never reached

-- |
--
-- >>> toErrorCodeId 0
-- NoError
-- >>> toErrorCodeId 0xc
-- InadequateSecurity
-- >>> toErrorCodeId 0xd
-- UnknownErrorCode 13
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
toErrorCodeId w   = UnknownErrorCode w

----------------------------------------------------------------

data SettingsId = SettingsHeaderTableSize
                | SettingsEnablePush
                | SettingsMaxConcurrentStreams
                | SettingsInitialWindowSize
                | SettingsMaxFrameSize -- this means payload size
                | SettingsMaxHeaderBlockSize
                deriving (Show, Read, Eq, Ord, Enum, Bounded, Ix)

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

type FrameTypeId = Word8

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
frameTypeToWord8 :: FrameType -> FrameTypeId
frameTypeToWord8 = fromIntegral . fromEnum

minFrameType :: FrameTypeId
minFrameType = fromIntegral $ fromEnum (minBound :: FrameType)

maxFrameType :: FrameTypeId
maxFrameType = fromIntegral $ fromEnum (maxBound :: FrameType)

-- |
--
-- >>> frameTypeFromWord8 0
-- Just FrameData
-- >>> frameTypeFromWord8 9
-- Just FrameContinuation
-- >>> frameTypeFromWord8 10
-- Nothing
frameTypeFromWord8 :: FrameTypeId -> Maybe FrameType
frameTypeFromWord8 x
  | minFrameType <= x && x <= maxFrameType = Just . toEnum . fromIntegral $ x
  | otherwise                              = Nothing

----------------------------------------------------------------

type PayloadLength = Int -- Word24 but Int is more natural

maxPayloadLength :: PayloadLength
maxPayloadLength = 2^(14::Int)

----------------------------------------------------------------
-- Flags

type FrameFlags = Word8

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

newtype StreamIdentifier = StreamIdentifier Word32 deriving (Show, Read, Eq)
type StreamDependency    = StreamIdentifier
type LastStreamId        = StreamIdentifier
type PromisedStreamId    = StreamIdentifier

toStreamIdentifier :: Word32 -> StreamIdentifier
toStreamIdentifier w = StreamIdentifier (w `clearBit` 31)

fromStreamIdentifier :: StreamIdentifier -> Word32
fromStreamIdentifier (StreamIdentifier w32) = w32

testExclusive :: Word32 -> Bool
testExclusive w = w `testBit` 31

setExclusive :: Word32 -> Word32
setExclusive w = w `setBit` 31

streamIdentifierForSeetings :: StreamIdentifier
streamIdentifierForSeetings = StreamIdentifier 0

----------------------------------------------------------------

type WindowSizeIncrement = Word32
type HeaderBlockFragment = ByteString
type Padding = ByteString

----------------------------------------------------------------

data Frame = Frame
    { frameHeader  :: FrameHeader
    , framePayload :: FramePayload
    } deriving (Show, Read, Eq)

-- A complete frame header
data FrameHeader = FrameHeader
    { payloadLength :: PayloadLength
    , flags         :: FrameFlags
    , streamId      :: StreamIdentifier
    } deriving (Show, Read, Eq)

data FramePayload =
    DataFrame ByteString
  | HeadersFrame (Maybe Priority) HeaderBlockFragment
  | PriorityFrame Priority
  | RSTStreamFrame ErrorCodeId
  | SettingsFrame Settings
  | PushPromiseFrame PromisedStreamId HeaderBlockFragment
  | PingFrame ByteString
  | GoAwayFrame LastStreamId ErrorCodeId ByteString
  | WindowUpdateFrame WindowSizeIncrement
  | ContinuationFrame HeaderBlockFragment
  | UnknownFrame FrameTypeId ByteString
  deriving (Show, Read, Eq)

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
  } deriving (Show, Read, Eq)

----------------------------------------------------------------

framePayloadToFrameTypeId :: FramePayload -> FrameTypeId
framePayloadToFrameTypeId (DataFrame _)          = frameTypeToWord8 FrameData
framePayloadToFrameTypeId (HeadersFrame _ _)     = frameTypeToWord8 FrameHeaders
framePayloadToFrameTypeId (PriorityFrame _)      = frameTypeToWord8 FramePriority
framePayloadToFrameTypeId (RSTStreamFrame _)     = frameTypeToWord8 FrameRSTStream
framePayloadToFrameTypeId (SettingsFrame _)      = frameTypeToWord8 FrameSettings
framePayloadToFrameTypeId (PushPromiseFrame _ _) = frameTypeToWord8 FramePushPromise
framePayloadToFrameTypeId (PingFrame _)          = frameTypeToWord8 FramePing
framePayloadToFrameTypeId (GoAwayFrame _ _ _)    = frameTypeToWord8 FrameGoAway
framePayloadToFrameTypeId (WindowUpdateFrame _)  = frameTypeToWord8 FrameWindowUpdate
framePayloadToFrameTypeId (ContinuationFrame _)  = frameTypeToWord8 FrameContinuation
framePayloadToFrameTypeId (UnknownFrame w8 _)    = w8
