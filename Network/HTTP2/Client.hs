{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Client (
    openHTTP2Connection
  , Connection
  , Request(..)
  , defaultRequest
  , Response(..)
  , withResponse
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (void, forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Network.Socket hiding (Stream)
import qualified Network.Socket.ByteString as NSB

import Network.HPACK (HeaderList, HeaderTable)
import qualified Network.HPACK as HPACK
import Network.HPACK.Token
import Network.HTTP.Types
import Network.HTTP2

data Connection = Connection {
    sendFrame    :: (FrameFlags -> FrameFlags) -> Int -> FramePayload -> IO ()
  , recvFrame    :: IO Frame
  , encodeHeader :: HeaderList -> IO ByteString
  , decodeHeader :: ByteString -> IO HeaderTable
  , streamTable  :: TVar (IntMap Stream)
  , streamNumber :: TVar Int
  , requestQ     :: TQueue (StreamId, Request)
  }

data Stream = Stream {
    responseQ :: TQueue Rsp
  }

type EndOfStream = Bool

data Rsp = Header  EndOfStream HeaderTable
         | Body    EndOfStream ByteString
--         | Trailer EndOfStream HeaderTable

----------------------------------------------------------------

openHTTP2Connection :: Socket -> IO Connection
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
    tbl <- newTVarIO I.empty
    num <- newTVarIO 1
    reqQ <- newTQueueIO
    let conn = Connection send recv enc dec tbl num reqQ
    _ <- forkIO $ receiver conn
    _ <- forkIO $ sender conn
    return conn

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

sender :: Connection -> IO ()
sender conn@Connection{..} = forever $ do
    (sid, req) <- atomically $ readTQueue requestQ
    sendRequest conn sid req

sendRequest :: Connection -> StreamId -> Request -> IO ()
sendRequest conn sid (Request m a p s h _b) = do
    hdrblk <- encodeHeader conn hdr'
    sendFrame conn (setEndHeader.setEndStream) sid $ HeadersFrame Nothing hdrblk
  where
    hdr = (":method", m)
        : (":authority", a)
        : (":path", p)
        : (":scheme", s)
        : h
    hdr' = map (\(k,v) -> (CI.foldedCase k,v)) hdr

----------------------------------------------------------------

receiver :: Connection -> IO ()
receiver Connection{..} = forever $ do
    Frame{..} <- recvFrame
    let FrameHeader{..} = frameHeader
    tbl <- atomically $ readTVar streamTable
    case I.lookup streamId tbl of
      Nothing -> return ()
      Just Stream{..} -> case framePayload of
          DataFrame bs -> do
              let endStream = testEndStream flags
              atomically $ writeTQueue responseQ $ Body endStream bs
          HeadersFrame _ hdrblk -> do
              header <- decodeHeader hdrblk
              let endStream = testEndStream flags
              atomically $ writeTQueue responseQ $ Header endStream header
          PriorityFrame _ -> return ()
          RSTStreamFrame _ -> return ()
          SettingsFrame _ -> return ()
          PushPromiseFrame _ _ -> return ()
          PingFrame _ -> return ()
          GoAwayFrame _ _ _ -> return ()
          WindowUpdateFrame _ -> return ()
          ContinuationFrame _ -> return ()
          UnknownFrame _ _ -> return ()

----------------------------------------------------------------

initialSettingFrame :: FramePayload
initialSettingFrame = SettingsFrame [
    (SettingsMaxConcurrentStreams,recommendedConcurrency)
  ]

ackSettingsFrame :: FramePayload
ackSettingsFrame = SettingsFrame []

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

----------------------------------------------------------------

data Response = Response {
    responseStatus :: Status
  , responseHeader :: HeaderTable
  , responseBody   :: [ByteString]
  } deriving Eq

instance Show Response where
    show (Response st (thl,_) body) =
        "Response " ++ show (statusCode st) ++ " " ++ show thl ++ " " ++ show body

----------------------------------------------------------------

withResponse :: Connection -> Request -> (Response -> IO a) -> IO a
withResponse Connection{..} req f = do
    q <- atomically $ do
        n <- readTVar streamNumber
        let n' = n + 2
        writeTVar streamNumber n'
        rspQ <- newTQueue
        let strm = Stream rspQ
        modifyTVar' streamTable $ \q -> I.insert n strm q
        writeTQueue requestQ (n,req)
        return rspQ
    response <- recvResponse q
    f response
  where
    recvResponse q = do
        Header end ht@(_,vt) <- atomically $ readTQueue q
        let Just status = HPACK.getHeaderValue tokenStatus vt
            st = toEnum $ read $ C8.unpack status
        if end then
            return $ Response st ht []
          else do
            body <- recvResponseBody q
            return $ Response st ht body
    recvResponseBody q = go id
      where
        go builder = do
            Body end dat <- atomically $ readTQueue q
            let builder' = (dat :) . builder
            if end then do
                return $ builder' []
              else
                go builder'
