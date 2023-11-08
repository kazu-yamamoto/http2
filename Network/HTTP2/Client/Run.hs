{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Client.Run where

import Control.Concurrent.STM (check)
import Control.Exception
import Data.ByteString.Builder (Builder)
import Network.Socket (SockAddr)
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.STM

import Imports
import Network.HTTP2.Arch
import Network.HTTP2.Client.Types
import Network.HTTP2.Frame

-- | Client configuration
data ClientConfig = ClientConfig
    { scheme :: Scheme
    -- ^ https or http
    , authority :: Authority
    -- ^ Server name
    , cacheLimit :: Int
    -- ^ How many pushed responses are contained in the cache
    }

-- | Running HTTP/2 client.
run :: ClientConfig -> Config -> Client a -> IO a
run cconf@ClientConfig{..} conf client = do
    (ctx, mgr) <- setup cconf conf
    let runClient = do
            x <- client $ \req processRequest -> do
                strm <- sendRequest ctx mgr scheme authority req
                rsp <- Response <$> takeMVar (streamInput strm)
                processRequest rsp
            waitCounter0 mgr
            let frame = goawayFrame 0 NoError "graceful closing"
            mvar <- newMVar ()
            enqueueControl (controlQ ctx) $ CGoaway frame mvar
            takeMVar mvar
            return x
    runArch conf ctx mgr runClient

runIO :: ClientConfig -> Config -> (ClientIO -> IO (IO a)) -> IO a
runIO cconf@ClientConfig{..} conf@Config{..} action = do
    (ctx@Context{..}, mgr) <- setup cconf conf
    let putB bs = enqueueControl controlQ $ CFrames Nothing [bs]
        putR req = do
            strm <- sendRequest ctx mgr scheme authority req
            return (streamNumber strm, strm)
        get strm = Response <$> takeMVar (streamInput strm)
        create = do
            sid <- getMyNewStreamId ctx
            strm <- openOddStream ctx sid FrameHeaders
            return (sid, strm)
    runClient <-
        action $ ClientIO confMySockAddr confPeerSockAddr putR get putB create
    runArch conf ctx mgr runClient

setup :: ClientConfig -> Config -> IO (Context, Manager)
setup ClientConfig{..} conf@Config{..} = do
    clientInfo <- newClientInfo scheme authority cacheLimit
    ctx <- newContext clientInfo confBufferSize confMySockAddr confPeerSockAddr
    mgr <- start confTimeoutManager
    exchangeSettings conf ctx
    return (ctx, mgr)

runArch :: Config -> Context -> Manager -> IO a -> IO a
runArch conf ctx mgr runClient =
    stopAfter mgr (race runBackgroundThreads runClient) $ \res -> do
        closeAllStreams (oddStreamTable ctx) $ either Just (const Nothing) res
        case res of
            Left err ->
                throwIO err
            Right (Left ()) ->
                undefined -- never reach
            Right (Right x) ->
                return x
  where
    runReceiver = frameReceiver ctx conf
    runSender = frameSender ctx conf mgr
    runBackgroundThreads = concurrently_ runReceiver runSender

sendRequest
    :: Context
    -> Manager
    -> Scheme
    -> Authority
    -> Request
    -> IO Stream
sendRequest ctx@Context{..} mgr scheme auth (Request req) = do
    -- Checking push promises
    let hdr0 = outObjHeaders req
        method = fromMaybe (error "sendRequest:method") $ lookup ":method" hdr0
        path = fromMaybe (error "sendRequest:path") $ lookup ":path" hdr0
    mstrm0 <- lookupCache method path roleInfo
    case mstrm0 of
        Just strm0 -> do
            deleteCache method path roleInfo
            return strm0
        Nothing -> do
            -- Arch/Sender is originally implemented for servers where
            -- the ordering of responses can be out-of-order.
            -- But for clients, the ordering must be maintained.
            -- To implement this, 'outputQStreamID' is used.
            -- Also, for 'OutBodyStreaming', TBQ must not be empty
            -- when its 'Output' is enqueued into 'outputQ'.
            -- Otherwise, it would be re-enqueue because of empty
            -- resulting in out-of-order.
            -- To implement this, 'tbqNonEmpty' is used.
            let hdr1
                    | scheme /= "" = (":scheme", scheme) : hdr0
                    | otherwise = hdr0
                hdr2
                    | auth /= "" = (":authority", auth) : hdr1
                    | otherwise = hdr1
                req' = req{outObjHeaders = hdr2}
            sid <- getMyNewStreamId ctx
            -- XXX
            -- Clinet: Peer SETTINGS_MAX_CONCURRENT_STREAMS
            newstrm <- openOddStream ctx sid FrameHeaders
            case outObjBody req of
                OutBodyStreaming strmbdy ->
                    sendStreaming ctx mgr req' sid newstrm $ \unmask push flush ->
                        unmask $ strmbdy push flush
                OutBodyStreamingUnmask strmbdy ->
                    sendStreaming ctx mgr req' sid newstrm strmbdy
                _ -> atomically $ do
                    sidOK <- readTVar outputQStreamID
                    check (sidOK == sid)
                    writeTVar outputQStreamID (sid + 2)
                    writeTQueue outputQ $ Output newstrm req' OObj Nothing (return ())
            return newstrm

sendStreaming
    :: Context
    -> Manager
    -> OutObj
    -> StreamId
    -> Stream
    -> ((forall x. IO x -> IO x) -> (Builder -> IO ()) -> IO () -> IO ())
    -> IO ()
sendStreaming Context{..} mgr req sid newstrm strmbdy = do
    tbq <- newTBQueueIO 10 -- fixme: hard coding: 10
    tbqNonEmpty <- newTVarIO False
    forkManagedUnmask mgr $ \unmask -> do
        let push b = atomically $ do
                writeTBQueue tbq (StreamingBuilder b)
                writeTVar tbqNonEmpty True
            flush = atomically $ writeTBQueue tbq StreamingFlush
            finished = atomically $ writeTBQueue tbq $ StreamingFinished (decCounter mgr)
        incCounter mgr
        strmbdy unmask push flush `finally` finished
    atomically $ do
        sidOK <- readTVar outputQStreamID
        ready <- readTVar tbqNonEmpty
        check (sidOK == sid && ready)
        writeTVar outputQStreamID (sid + 2)
        writeTQueue outputQ $ Output newstrm req OObj (Just tbq) (return ())

exchangeSettings :: Config -> Context -> IO ()
exchangeSettings conf ctx@Context{..} = do
    frames <- updateMySettings conf ctx
    let setframe = CFrames Nothing (connectionPreface : frames)
    enqueueControl controlQ setframe

data ClientIO = ClientIO
    { cioMySockAddr :: SockAddr
    , cioPeerSockAddr :: SockAddr
    , cioWriteRequest :: Request -> IO (StreamId, Stream)
    , cioReadResponse :: Stream -> IO Response
    , cioWriteBytes :: ByteString -> IO ()
    , cioCreateStream :: IO (StreamId, Stream)
    }
