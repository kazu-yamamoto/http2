{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2.Client (
    openHTTP2Connection
  , Context(..)
  ) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Network.HPACK (HeaderList)
import qualified Network.HPACK as HPACK
import Network.HTTP2
import Network.Socket
import qualified Network.Socket.ByteString as NSB

data Context = Context {
    sendFrame    :: (FrameFlags -> FrameFlags) -> Int -> FramePayload -> IO ()
  , recvFrame    :: IO Frame
  , encodeHeader :: HeaderList -> IO ByteString
  , decodeHeader :: ByteString -> IO HeaderList
  }

----------------------------------------------------------------

openHTTP2Connection :: Socket -> IO Context
openHTTP2Connection sock = do
    etbl <- HPACK.newDynamicTableForEncoding HPACK.defaultDynamicTableSize
    dtbl <- HPACK.newDynamicTableForDecoding HPACK.defaultDynamicTableSize 4096
    NSB.sendAll sock connectionPreface
    sendFrame' sock id 0 initialSettingFrame
    void $ recvFrame' sock
    void $ recvFrame' sock
    sendFrame' sock setAck 0 ackSettingsFrame
    let enc = HPACK.encodeHeader HPACK.defaultEncodeStrategy 4096 etbl
        dec = HPACK.decodeHeader dtbl
        send = sendFrame' sock
        recv = recvFrame' sock
    return $ Context send recv enc dec

initialSettingFrame :: FramePayload
initialSettingFrame = SettingsFrame [
    (SettingsMaxConcurrentStreams,recommendedConcurrency)
  ]

ackSettingsFrame :: FramePayload
ackSettingsFrame = SettingsFrame []

----------------------------------------------------------------

sendFrame' :: Socket -> (FrameFlags -> FrameFlags) -> Int -> FramePayload -> IO ()
sendFrame' sock func sid payload = do
    let einfo = encodeInfo func sid
        frame = encodeFrame einfo payload
    NSB.sendAll sock frame

recvFrame' :: Socket -> IO Frame
recvFrame' sock = do
    (frameId, header) <- decodeFrameHeader <$> NSB.recv sock frameHeaderLength
    let len = payloadLength header
    body <- if len == 0 then return "" else NSB.recv sock len
    let Right payload = decodeFramePayload frameId header body
    return $ Frame header payload
