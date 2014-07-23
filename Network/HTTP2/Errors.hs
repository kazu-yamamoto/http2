module Network.HTTP2.Errors
    (
      -- * Errors
      ErrorCode (..)
    , errorCodeToWord32
    , errorCodeFromWord32
    ) where

import qualified Data.Map  as Map
import           Data.Word (Word32)

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

errorCodeToWord32 :: ErrorCode -> Word32
errorCodeToWord32 NoError = 0x0
errorCodeToWord32 ProtocolError = 0x1
errorCodeToWord32 InternalError = 0x2
errorCodeToWord32 FlowControlError = 0x3
errorCodeToWord32 SettingsTimeout = 0x4
errorCodeToWord32 StreamClosed = 0x5
errorCodeToWord32 FrameSizeError = 0x6
errorCodeToWord32 RefusedStream = 0x7
errorCodeToWord32 Cancel = 0x8
errorCodeToWord32 CompressionError = 0x9
errorCodeToWord32 ConnectError = 0xa
errorCodeToWord32 EnhanceYourCalm = 0xb
errorCodeToWord32 InadequateSecurity = 0xc

errorCodeFromWord32 :: Word32 -> Maybe ErrorCode
errorCodeFromWord32 =
    flip Map.lookup m
  where
    m = Map.fromList $ map (\e -> (errorCodeToWord32 e, e)) [minBound..maxBound]
