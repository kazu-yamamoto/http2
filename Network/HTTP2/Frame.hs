{-# LANGUAGE OverloadedStrings #-}

-- | Framing in HTTP\/2(<https://tools.ietf.org/html/rfc7540>).
module Network.HTTP2.Frame (
  -- * Frame
    Frame(..)
  , FrameHeader(..)
  , FramePayload(..)
  , HeaderBlockFragment
  , Padding
  , isPaddingDefined
  -- * Encoding
  , encodeFrame
  , encodeFrameChunks
  , encodeFrameHeader
  , encodeFrameHeaderBuf
  , encodeFramePayload
  , EncodeInfo(..)
  , encodeInfo
  , module Network.HTTP2.Frame.Decode
  -- * Frame type
  , FrameType(FrameType,FrameData,FrameHeaders,FramePriority,FrameRSTStream,FrameSettings,FramePushPromise,FramePing,FrameGoAway,FrameWindowUpdate,FrameContinuation)
  , fromFrameType
  , toFrameType
  , minFrameType
  , maxFrameType
  , framePayloadToFrameType
  -- * Priority
  , Priority(..)
  , Weight
  , defaultPriority
  , highestPriority
  , defaultWeight
  -- * Stream identifier
  , StreamId
  , isControl
  , isClientInitiated
  , isServerInitiated
  , isRequest
  , isResponse
  -- * Stream identifier related
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
  -- * SettingsList
  , SettingsList
  , SettingsKey(SettingsKey,SettingsHeaderTableSize,SettingsEnablePush,SettingsMaxConcurrentStreams,SettingsInitialWindowSize,SettingsMaxFrameSize,SettingsMaxHeaderBlockSize)
  , SettingsValue
  , fromSettingsKey
  , toSettingsKey
  , checkSettingsList
  -- * Settings
  , Settings(..)
  , defaultSettings
  , updateSettings
  -- * Window
  , WindowSize
  , defaultInitialWindowSize
  , maxWindowSize
  , isWindowOverflow
  -- * Error code
  , ErrorCode(ErrorCode,NoError,ProtocolError,InternalError,FlowControlError,SettingsTimeout,StreamClosed,FrameSizeError,RefusedStream,Cancel,CompressionError,ConnectError,EnhanceYourCalm,InadequateSecurity,HTTP11Required)
  , fromErrorCode
  , toErrorCode
  -- * Error
  , HTTP2Error(..)
  -- * Predefined values
  , connectionPreface
  , connectionPrefaceLength
  , frameHeaderLength
  , maxPayloadLength
  , recommendedConcurrency
  ) where

import qualified Data.ByteString as BS

import Imports
import Network.HTTP2.Frame.Decode
import Network.HTTP2.Frame.Encode
import Network.HTTP2.Frame.Types

-- | The preface of HTTP\/2.
--
-- >>> connectionPreface
-- "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
connectionPreface :: ByteString
connectionPreface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

-- | Length of the preface.
--
-- >>> connectionPrefaceLength
-- 24
connectionPrefaceLength :: Int
connectionPrefaceLength = BS.length connectionPreface
