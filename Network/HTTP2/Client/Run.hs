{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Client.Run where

import Control.Concurrent.STM (check)
import Control.Exception
import Data.ByteString.Builder (Builder)
import Data.IORef
import Network.Socket (SockAddr)
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.STM

import Imports
import Network.HTTP2.Client.Types
import Network.HTTP2.Frame
import Network.HTTP2.H2

-- | Client configuration
data ClientConfig = ClientConfig
    { scheme :: Scheme
    -- ^ https or http
    , authority :: Authority
    -- ^ Server name
    , cacheLimit :: Int
    -- ^ How many pushed responses are contained in the cache
    , concurrentStreams :: Int
    -- ^ The maximum number of incoming streams on the net
    , windowSize :: WindowSize
    -- ^ The window size of incoming streams
    }
    deriving (Eq, Show)

-- | The default client config.
--
-- >>> defaultClientConfig
-- ClientConfig {scheme = "http", authority = "localhost", cacheLimit = 64, concurrentStreams = 64, windowSize = 1048575}
defaultClientConfig :: ClientConfig
defaultClientConfig =
    ClientConfig
        { scheme = "http"
        , authority = "localhost"
        , cacheLimit = 64
        , concurrentStreams = properConcurrentStreams
        , windowSize = properWindowSize
        }

-- | Running HTTP/2 client.
run :: ClientConfig -> Config -> Client a -> IO a
run cconf@ClientConfig{..} conf client = do
    (ctx, mgr) <- setup cconf conf
    let runClient = do
            x <- client $ \req processRequest -> do
                strm <- sendRequest ctx mgr scheme authority req
                rsp <- getResponse strm
                let serverMaxStreams = maxConcurrentStreams <$> readIORef (peerSettings ctx)
                    possibleClientStream = do
                        mx <- serverMaxStreams
                        case mx of
                            Nothing -> return Nothing
                            Just x -> do
                                n <- oddConc <$> readTVarIO (oddStreamTable ctx)
                                return $ Just (x - n)
                    aux =
                        Aux
                            { auxPossibleClientStreams = possibleClientStream
                            , auxServerMaxStreams = serverMaxStreams
                            }
                processRequest rsp aux
            waitCounter0 mgr
            let frame = goawayFrame 0 NoError "graceful closing"
            mvar <- newMVar ()
            enqueueControl (controlQ ctx) $ CGoaway frame mvar
            takeMVar mvar
            return x
    runH2 conf ctx mgr runClient

-- | Launching a receiver and a sender.
runIO :: ClientConfig -> Config -> (ClientIO -> IO (IO a)) -> IO a
runIO cconf@ClientConfig{..} conf@Config{..} action = do
    (ctx@Context{..}, mgr) <- setup cconf conf
    let putB bs = enqueueControl controlQ $ CFrames Nothing [bs]
        putR req = do
            strm <- sendRequest ctx mgr scheme authority req
            return (streamNumber strm, strm)
        get = getResponse
        create = openOddStreamWait ctx
    runClient <-
        action $ ClientIO confMySockAddr confPeerSockAddr putR get putB create
    runH2 conf ctx mgr runClient

getResponse :: Stream -> IO Response
getResponse strm = do
    mRsp <- takeMVar $ streamInput strm
    case mRsp of
        Left err -> throwIO err
        Right rsp -> return $ Response rsp

setup :: ClientConfig -> Config -> IO (Context, Manager)
setup ClientConfig{..} conf@Config{..} = do
    let clientInfo = newClientInfo scheme authority
        myAlist = makeMySettingsList conf concurrentStreams windowSize
    ctx <-
        newContext
            clientInfo
            cacheLimit
            confBufferSize
            confMySockAddr
            confPeerSockAddr
            myAlist
            windowSize
    mgr <- start confTimeoutManager
    exchangeSettings ctx
    return (ctx, mgr)

runH2 :: Config -> Context -> Manager -> IO a -> IO a
runH2 conf ctx mgr runClient =
    stopAfter mgr (race runBackgroundThreads runClient) $ \res -> do
        closeAllStreams (oddStreamTable ctx) (evenStreamTable ctx) $
            either Just (const Nothing) res
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
    mstrm0 <- lookupEvenCache evenStreamTable method path
    case mstrm0 of
        Just strm0 -> do
            deleteEvenCache evenStreamTable method path
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
            -- FLOW CONTROL: SETTINGS_MAX_CONCURRENT_STREAMS: send: respecting peer's limit
            (sid, newstrm) <- openOddStreamWait ctx
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

exchangeSettings :: Context -> IO ()
exchangeSettings ctx@Context{..} = do
    frames <- pendingMySettings ctx
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
