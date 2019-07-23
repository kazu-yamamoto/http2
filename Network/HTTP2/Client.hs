{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Client (
    Connection
  , Request(..)
  , defaultRequest
  , Response(..)
  , withConnection
  , withResponse
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (void, forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Network.Socket hiding (Stream)
import qualified Network.Socket.ByteString as NSB

import Network.HPACK (HeaderList, HeaderTable)
import qualified Network.HPACK as HPACK
import Network.HPACK.Token
import Network.HTTP.Types
import Network.HTTP2

----------------------------------------------------------------

-- | HTTP\/2 request for clients.
data Request = Request {
      requestMethod    :: Method
    , requestAuthority :: ByteString
    , requestPath      :: ByteString
    , requestScheme    :: ByteString
    , requestHeaders   :: RequestHeaders
    , requestBody      :: IO ByteString
    }

-- | Default request.
defaultRequest :: Request
defaultRequest = Request {
      requestMethod    = methodGet
    , requestAuthority = "127.0.0.1"
    , requestPath      = "/"
    , requestScheme    = "http"
    , requestHeaders   = []
    , requestBody      = return ""
    }

----------------------------------------------------------------

-- | HTTP\/2 response for clients.
data Response = Response {
    responseStatus  :: Status
  , responseHeader  :: HeaderTable
  , responseBody    :: IO ByteString
  , responseTrailer :: IORef (Maybe HeaderTable)
  }

instance Show Response where
    show (Response st (thl,_) _body _tref) =
        "Response " ++ show (statusCode st) ++ " " ++ show thl ++ " "

----------------------------------------------------------------

-- | HTTP\/2 connection.
data Connection = Connection {
    streamNumber   :: TVar Int
  , requestQ       :: RequestQ
  , responseQTable :: ResponseQTable
  }

type RequestQ  = TQueue Req
type ResponseQ = TQueue Rsp
type ResponseQTable = TVar (IntMap ResponseQ)

data Req = ReqHeader StreamId Request
         | ReqBody   StreamId ByteString (IO ByteString)
data Rsp = RspHeader  EndOfStream HeaderTable
         | RspBody    EndOfStream ByteString
--       | RspTrailer EndOfStream HeaderTable
type EndOfStream = Bool

type SendFrame = (FrameFlags -> FrameFlags) -> Int -> FramePayload -> IO ()
type RecvFrame = IO Frame

type EncodeHeader = HeaderList -> IO ByteString
type DecodeHeader = ByteString -> IO HeaderTable

----------------------------------------------------------------

newStream :: Connection -> Request -> IO (StreamId, ResponseQ)
newStream Connection{..} req = atomically $ do
    n <- readTVar streamNumber
    let n' = n + 2
    writeTVar streamNumber n'
    rspQ <- newTQueue
    modifyTVar' responseQTable $ \q -> I.insert n rspQ q
    writeTQueue requestQ $ ReqHeader n req
    return (n, rspQ)

deleteStream :: Connection -> StreamId -> IO ()
deleteStream Connection{..} n = atomically $
    modifyTVar' responseQTable $ \q -> I.delete n q

-- | Sending an HTTP\/2 request and passing its response
--   to an action.
withResponse :: Connection -> Request -> (Response -> IO a) -> IO a
withResponse conn req f = do
    (sid, q) <- newStream conn req
    response <- recvResponse q
    ret <- f response
    deleteStream conn sid
    return ret
  where
    recvResponse q = do
        RspHeader end ht@(_,vt) <- atomically $ readTQueue q
        let Just status = HPACK.getHeaderValue tokenStatus vt
            st = toEnum $ read $ C8.unpack status
        trailerRef <- IORef.newIORef Nothing
        if end then
            return $ Response st ht (return "") trailerRef
          else do
            endRef <- IORef.newIORef False
            return $ Response st ht (recvResponseBody q endRef trailerRef) trailerRef

recvResponseBody :: ResponseQ -> IORef Bool -> IORef (Maybe HeaderTable)
                 -> IO ByteString
recvResponseBody q endRef trailerRef = do
    finished <- IORef.readIORef endRef
    if finished then
        return ""
      else do
        rsp <- atomically $ readTQueue q
        case rsp of
          RspBody end dat
            | end -> do
                  IORef.writeIORef endRef True
                  return dat
            | otherwise -> do
                  if dat == "" then
                       recvResponseBody q endRef trailerRef
                     else
                       return dat
          RspHeader _end ht -> do -- fixme: not suport continuation
              IORef.writeIORef endRef True
              IORef.writeIORef trailerRef $ Just ht
              return ""

----------------------------------------------------------------

-- | Creating an HTTP\/2 connection with a socket and
--   running an action.
withConnection :: Socket -> (Connection -> IO a) -> IO a
withConnection sock body = E.bracket (openHTTP2Connection sock)
                                     teardown
                                     (\(conn,_,_) -> body conn)
  where
    teardown (_,tid1,tid2) = do
        killThread tid1
        killThread tid2

openHTTP2Connection :: Socket -> IO (Connection, ThreadId, ThreadId)
openHTTP2Connection sock = do
    exchangeSettings sock
    (enc,dec) <- newDynamicTables
    conn@Connection{..} <- newConnection
    tid1 <- forkIO $ sender   enc (sendFrame sock) requestQ
    tid2 <- forkIO $ receiver dec (recvFrame sock) responseQTable
    return (conn,tid1,tid2)

exchangeSettings :: Socket -> IO ()
exchangeSettings sock = do
    NSB.sendAll sock connectionPreface
    sendFrame sock id 0 initialSettingFrame
    void $ recvFrame sock
    void $ recvFrame sock
    sendFrame sock setAck 0 ackSettingsFrame

newDynamicTables :: IO (EncodeHeader, DecodeHeader)
newDynamicTables = do
    etbl <- HPACK.newDynamicTableForEncoding HPACK.defaultDynamicTableSize
    dtbl <- HPACK.newDynamicTableForDecoding HPACK.defaultDynamicTableSize 4096
    let enc = HPACK.encodeHeader HPACK.defaultEncodeStrategy 4096 etbl
        dec = HPACK.decodeTokenHeader dtbl
    return (enc, dec)

newConnection :: IO Connection
newConnection = Connection <$> newTVarIO 1
                           <*> newTQueueIO
                           <*> newTVarIO I.empty

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

sender :: EncodeHeader -> SendFrame -> RequestQ -> IO ()
sender enc send requestQ = forever $ do
    req <- atomically $ readTQueue requestQ
    case req of
      ReqHeader sid (Request m a p s h body) -> do
          let hdr = (":method", m)
                  : (":authority", a)
                  : (":path", p)
                  : (":scheme", s)
                  : h
              hdr' = map (\(k,v) -> (CI.foldedCase k,v)) hdr
          hdrblk <- enc hdr'
          bs1 <- body
          if bs1 == "" then
              send (setEndHeader.setEndStream) sid $ HeadersFrame Nothing hdrblk
            else do
              send setEndHeader sid $ HeadersFrame Nothing hdrblk
              atomically $ writeTQueue requestQ $ ReqBody sid bs1 body
      ReqBody sid bs0 body -> do
          bs1 <- body
          if bs1 == "" then
              send setEndStream sid $ DataFrame bs0
            else do
              send id sid $ DataFrame bs0
              atomically $ writeTQueue requestQ $ ReqBody sid bs1 body

----------------------------------------------------------------

receiver :: DecodeHeader -> RecvFrame -> ResponseQTable -> IO ()
receiver dec recv qtbl = forever $ do
    Frame{..} <- recv
    let FrameHeader{..} = frameHeader
    tbl <- atomically $ readTVar qtbl
    case I.lookup streamId tbl of
      Nothing -> return ()
      Just responseQ -> case framePayload of
          DataFrame bs -> do
              let endStream = testEndStream flags
              atomically $ writeTQueue responseQ $ RspBody endStream bs
          HeadersFrame _ hdrblk -> do
              header <- dec hdrblk
              let endStream = testEndStream flags
              atomically $ writeTQueue responseQ $ RspHeader endStream header
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
