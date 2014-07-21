module Network.HTTP2.Frames
    (
    ) where

import           Control.Applicative        ((<$>), (<*>))
import qualified Data.Attoparsec.Binary     as BI
import qualified Data.Attoparsec.ByteString as B
import           Data.Bits                  (shiftL, clearBit, (.|.))
import           Data.Int                   (Int32)
import           Data.List                  (foldl')
import qualified Data.Map                   as Map
import           Data.Word                  (Word16, Word32, Word8)

type Int24 = Int32

data SettingID = SettingHeaderTableSize
               | SettingEnablePush
               | SettingMaxConcurrentStreams
               | SettingInitialWindowSize
               | SettingUnknown
               deriving (Show, Eq, Ord, Enum, Bounded)

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

data FrameHeader = FrameHeader
    { fhType     :: FrameType
    , fhFlags    :: Word8
    , fhLength   :: Int24
    , fhStreamId :: Word32
    } deriving (Show, Eq)

settingIdToWord16 :: SettingID -> Word16
settingIdToWord16 SettingHeaderTableSize      = 0x1
settingIdToWord16 SettingEnablePush           = 0x2
settingIdToWord16 SettingMaxConcurrentStreams = 0x3
settingIdToWord16 SettingInitialWindowSize    = 0x4

settingIdFromWord16 :: Word16 -> SettingID
settingIdFromWord16 k =
    Map.findWithDefault SettingUnknown k m
  where
    m = Map.fromList $ map (\s -> (settingIdToWord16 s, s)) [minBound..maxBound]

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

frameTypeFromWord8 :: Word8 -> FrameType
frameTypeFromWord8 k =
    Map.findWithDefault FrameUnknown k m
  where
    m = Map.fromList $ map (\f -> (frameTypeToWord8 f, f)) [minBound..maxBound]

parseFrameHeader :: B.Parser FrameHeader
parseFrameHeader = do
    a <- B.anyWord8
    b <- B.anyWord8
    c <- B.anyWord8
    let length = roll [a, b, c]
    typ <- frameTypeFromWord8 <$> B.anyWord8
    flags <- B.anyWord8
    streamId <- flip clearBit 31 <$> BI.anyWord32be
    return $ FrameHeader typ flags length streamId

roll :: [Word8] -> Int24
roll = foldl' unstep 0
  where
    unstep a b = a `shiftL` 8 .|. fromIntegral b
