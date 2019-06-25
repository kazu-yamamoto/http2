{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2.Client (
    openHTTP2Connection
  , sendFrame
  , recvFrame
  , encodeHeader
  , decodeHeader
  ) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Network.HPACK (DynamicTable, HeaderList)
import qualified Network.HPACK as HPACK
import Network.HTTP2
import Network.Socket
import qualified Network.Socket.ByteString as NSB

----------------------------------------------------------------

openHTTP2Connection :: Socket -> IO (DynamicTable, DynamicTable)
openHTTP2Connection sock = do
    etbl <- HPACK.newDynamicTableForEncoding HPACK.defaultDynamicTableSize
    dtbl <- HPACK.newDynamicTableForDecoding HPACK.defaultDynamicTableSize 4096
    NSB.sendAll sock connectionPreface
    sendFrame sock id 0 initialSettingFrame
    void $ recvFrame sock
    void $ recvFrame sock
    sendFrame sock setAck 0 ackSettingsFrame
    return (etbl, dtbl)

initialSettingFrame :: FramePayload
initialSettingFrame = SettingsFrame [
    (SettingsMaxConcurrentStreams,recommendedConcurrency)
  ]

ackSettingsFrame :: FramePayload
ackSettingsFrame = SettingsFrame []

----------------------------------------------------------------

sendFrame :: Socket -> (FrameFlags -> FrameFlags) -> Int -> FramePayload -> IO ()
sendFrame sock func sid payload = do
    let einfo = encodeInfo func sid
        frame = encodeFrame einfo payload
    NSB.sendAll sock frame

recvFrame :: Socket -> IO Frame
recvFrame sock = do
    (frameId, header) <- decodeFrameHeader <$> NSB.recv sock frameHeaderLength
    let len = payloadLength header
    body <- if len == 0 then return "" else NSB.recv sock len
    let Right payload = decodeFramePayload frameId header body
    return $ Frame header payload

----------------------------------------------------------------

encodeHeader :: DynamicTable -> HeaderList -> IO ByteString
encodeHeader etbl = HPACK.encodeHeader HPACK.defaultEncodeStrategy 4096 etbl

decodeHeader :: DynamicTable -> ByteString -> IO HeaderList
decodeHeader dtbl = HPACK.decodeHeader dtbl
