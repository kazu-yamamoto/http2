{-# LANGUAGE OverloadedStrings #-}

-- | Framing in HTTP/2.
module Network.HTTP2 (
  -- * Frame
    Frame(..)
  , FrameHeader(..)
  , FramePayload(..)
  , isPaddingDefined
  -- * Encoding
  , encodeFrame
  , encodeFrameChunks
  , encodeFrameHeader
  , encodeFramePayload
  , EncodeInfo(..)
  , encodeInfo
  , module Network.HTTP2.Decode
  -- * Frame type ID
  , FrameTypeId(..)
  , framePayloadToFrameTypeId
  -- * Frame type
  , FrameType
  , fromFrameTypeId
  , toFrameTypeId
  -- * Types
  , HeaderBlockFragment
  , Padding
  , Priority(..)
  , PromisedStreamId
  , LastStreamId
  , StreamDependency
  , WindowSizeIncrement
  -- * Stream identifier
  , StreamIdentifier
  , fromStreamIdentifier
  , toStreamIdentifier
  , isControl
  , isRequest
  , isResponse
  -- * Stream identifier related
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
  -- * SettingsList
  , SettingsList
  , SettingsValue
  , SettingsKeyId(..)
  , fromSettingsKeyId
  , toSettingsKeyId
  , checkSettingsList
  -- * Settings
  , Settings(..)
  , defaultSettings
  , defaultInitialWindowSize
  , maxWindowSize
  , updateSettings
  -- * Error code
  , ErrorCode
  , ErrorCodeId(..)
  , fromErrorCodeId
  , toErrorCodeId
  -- * Error
  , HTTP2Error(..)
  , errorCodeId
  -- * Magic
  , connectionPreface
  , connectionPrefaceLength
  , frameHeaderLength
  , maxPayloadLength
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.HTTP2.Decode
import Network.HTTP2.Encode
import Network.HTTP2.Types

-- | The preface of HTTP/2.
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
