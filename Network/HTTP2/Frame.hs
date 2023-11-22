{-# LANGUAGE OverloadedStrings #-}

-- | Framing in HTTP\/2(<https://www.rfc-editor.org/rfc/rfc9113>).
module Network.HTTP2.Frame (
    -- * Frame
    Frame (..),
    FrameHeader (..),
    FramePayload (..),
    HeaderBlockFragment,
    Padding,
    isPaddingDefined,

    -- * Encoding
    encodeFrame,
    encodeFrameChunks,
    encodeFrameHeader,
    encodeFrameHeaderBuf,
    encodeFramePayload,
    EncodeInfo (..),
    encodeInfo,
    module Network.HTTP2.Frame.Decode,

    -- * Frame type
    FrameType (
        FrameType,
        FrameData,
        FrameHeaders,
        FramePriority,
        FrameRSTStream,
        FrameSettings,
        FramePushPromise,
        FramePing,
        FrameGoAway,
        FrameWindowUpdate,
        FrameContinuation
    ),
    fromFrameType,
    toFrameType,
    minFrameType,
    maxFrameType,
    framePayloadToFrameType,

    -- * Priority
    Priority (..),
    Weight,

    -- * Stream identifier
    StreamId,
    isControl,
    isClientInitiated,
    isServerInitiated,

    -- * Stream identifier related
    testExclusive,
    setExclusive,
    clearExclusive,

    -- * Flags
    FrameFlags,
    defaultFlags,
    testEndStream,
    testAck,
    testEndHeader,
    testPadded,
    testPriority,
    setEndStream,
    setAck,
    setEndHeader,
    setPadded,
    setPriority,

    -- * SettingsList
    SettingsList,
    SettingsKey (
        SettingsKey,
        SettingsHeaderTableSize,
        SettingsEnablePush,
        SettingsMaxConcurrentStreams,
        SettingsInitialWindowSize,
        SettingsMaxFrameSize,
        SettingsMaxHeaderListSize
    ),
    SettingsValue,
    fromSettingsKey,
    toSettingsKey,

    -- * Payload length
    defaultPayloadLength,
    maxPayloadLength,

    -- * Window
    WindowSize,
    defaultWindowSize,
    maxWindowSize,
    isWindowOverflow,

    -- * Error code
    ErrorCode (
        ErrorCode,
        NoError,
        ProtocolError,
        InternalError,
        FlowControlError,
        SettingsTimeout,
        StreamClosed,
        FrameSizeError,
        RefusedStream,
        Cancel,
        CompressionError,
        ConnectError,
        EnhanceYourCalm,
        InadequateSecurity,
        HTTP11Required
    ),
    fromErrorCode,
    toErrorCode,

    -- * Predefined values
    connectionPreface,
    connectionPrefaceLength,
    frameHeaderLength,
    recommendedConcurrency,

    -- * Deprecated
    ErrorCodeId,
    SettingsKeyId,
    FrameTypeId,
) where

import qualified Data.ByteString as BS
import Network.Control (WindowSize)

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
