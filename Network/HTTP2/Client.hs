{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2.Client (
    openHTTP2Connection
  , Context(..)
  , Request(..)
  , defaultRequest
  , Response(..)
  , sendRequest
  , recvResponse
  ) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import qualified Network.Socket.ByteString as NSB

import Network.HPACK (HeaderList, HeaderTable)
import qualified Network.HPACK as HPACK
import Network.HPACK.Token
import Network.HTTP.Types
import Network.HTTP2
import Network.Socket

data Context = Context {
    sendFrame    :: (FrameFlags -> FrameFlags) -> Int -> FramePayload -> IO ()
  , recvFrame    :: IO Frame
  , encodeHeader :: HeaderList -> IO ByteString
  , decodeHeader :: ByteString -> IO HeaderTable
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
        dec = HPACK.decodeTokenHeader dtbl
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

----------------------------------------------------------------

data Request = Request {
      requestMethod    :: Method
    , requestAuthority :: ByteString
    , requestPath      :: ByteString
    , requestScheme    :: ByteString
    , requestHeaders   :: RequestHeaders
    , requestBody      :: ByteString
    }

defaultRequest :: Request
defaultRequest = Request {
      requestMethod    = methodGet
    , requestAuthority = "127.0.0.1"
    , requestPath      = "/"
    , requestScheme    = "http"
    , requestHeaders   = []
    , requestBody      = ""
    }

sendRequest :: Context -> Request -> IO ()
sendRequest ctx (Request m a p s h _b) = do
    hdrblk <- encodeHeader ctx hdr'
    sendFrame ctx (setEndHeader.setEndStream) 1 $ HeadersFrame Nothing hdrblk
  where
    hdr = (":method", m)
        : (":authority", a)
        : (":path", p)
        : (":scheme", s)
        : h
    hdr' = map (\(k,v) -> (CI.foldedCase k,v)) hdr

----------------------------------------------------------------

data Response = Response {
    responseStatus :: Status
  , responseHeader :: HeaderTable
  , responseBody   :: [ByteString]
  } deriving Eq

instance Show Response where
    show (Response st (thl,_) body) =
        "Response " ++ show (statusCode st) ++ " " ++ show thl ++ " " ++ show body

recvResponse :: Context -> IO Response
recvResponse ctx = do
    (ht@(_,vt), endStream) <- recvResponseHeader ctx
    let Just status = HPACK.getHeaderValue tokenStatus vt
        st = toEnum $ read $ C8.unpack status
    if endStream then
        return $ Response st ht []
      else do
        body <- recvResponseBody ctx
        return $ Response st ht body

recvResponseHeader :: Context -> IO (HeaderTable, Bool)
recvResponseHeader ctx = do
    Frame fhdr (HeadersFrame _ hbf) <- recvFrame ctx
    header <- decodeHeader ctx hbf
    let endStream = testEndStream $ flags fhdr
    return (header, endStream)

-- fixme: assuming no trailers
recvResponseBody :: Context -> IO [ByteString]
recvResponseBody ctx = go id
  where
    go builder = do
        Frame fhdr (DataFrame dat) <- recvFrame ctx
        let builder' = (dat :) . builder
        if testEndStream $ flags fhdr then do
            return $ builder' []
          else
            go builder'
