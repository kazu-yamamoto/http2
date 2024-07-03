{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Client.Run where

import Control.Concurrent.STM (check)
import qualified Data.ByteString.UTF8 as UTF8
import Data.IORef
import Network.Control (RxFlow (..), defaultMaxData)
import Network.HTTP.Semantics.Client
import Network.HTTP.Semantics.Client.Internal
import Network.HTTP.Semantics.IO
import Network.Socket (SockAddr)
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.STM

import Imports
import Network.HTTP2.Frame
import Network.HTTP2.H2

-- | Client configuration
data ClientConfig = ClientConfig
    { scheme :: Scheme
    -- ^ https or http
    , authority :: Authority
    -- ^ Server name
    , cacheLimit :: Int
    -- ^ The maximum number of incoming streams on the net
    , connectionWindowSize :: WindowSize
    -- ^ The window size of connection.
    , settings :: Settings
    -- ^ Settings
    }
    deriving (Eq, Show)

-- | The default client config.
--
-- The @authority@ field will be used to set the HTTP2 @:authority@
-- pseudo-header. In most cases you will want to override it to be equal to
-- @host@.
--
-- Further background on @authority@:
-- [RFC 3986](https://datatracker.ietf.org/doc/html/rfc3986#section-3.2) also
-- allows @host:port@, and most servers will accept this too. However, when
-- using TLS, many servers will expect the TLS SNI server name and the
-- @:authority@ pseudo-header to be equal, and for TLS SNI the server name
-- should not include the port. Note that HTTP2 explicitly /disallows/ using
-- @userinfo\@@ as part of the authority.
--
-- >>> defaultClientConfig
-- ClientConfig {scheme = "http", authority = "localhost", cacheLimit = 64, connectionWindowSize = 1048576, settings = Settings {headerTableSize = 4096, enablePush = True, maxConcurrentStreams = Just 64, initialWindowSize = 262144, maxFrameSize = 16384, maxHeaderListSize = Nothing, pingRateLimit = 10}}
defaultClientConfig :: ClientConfig
defaultClientConfig =
    ClientConfig
        { scheme = "http"
        , authority = "localhost"
        , cacheLimit = 64
        , connectionWindowSize = defaultMaxData
        , settings = defaultSettings
        }

-- | Running HTTP/2 client.
run :: ClientConfig -> Config -> Client a -> IO a
run cconf@ClientConfig{..} conf client = do
    ctx <- setup cconf conf
    runH2 conf ctx $ runClient ctx
  where
    serverMaxStreams ctx = do
        mx <- maxConcurrentStreams <$> readIORef (peerSettings ctx)
        case mx of
            Nothing -> return maxBound
            Just x -> return x
    possibleClientStream ctx = do
        x <- serverMaxStreams ctx
        n <- oddConc <$> readTVarIO (oddStreamTable ctx)
        return (x - n)
    aux ctx =
        Aux
            { auxPossibleClientStreams = possibleClientStream ctx
            }
    clientCore ctx req processResponse = do
        strm <- sendRequest ctx scheme authority req
        rsp <- getResponse strm
        x <- processResponse rsp
        adjustRxWindow ctx strm
        return x
    runClient ctx = do
        x <- client (clientCore ctx) $ aux ctx
        waitCounter0 $ threadManager ctx
        let frame = goawayFrame 0 NoError "graceful closing"
        mvar <- newMVar ()
        enqueueControl (controlQ ctx) $ CGoaway frame mvar
        takeMVar mvar
        return x

-- | Launching a receiver and a sender.
runIO :: ClientConfig -> Config -> (ClientIO -> IO (IO a)) -> IO a
runIO cconf@ClientConfig{..} conf@Config{..} action = do
    ctx@Context{..} <- setup cconf conf
    let putB bs = enqueueControl controlQ $ CFrames Nothing [bs]
        putR req = do
            strm <- sendRequest ctx scheme authority req
            return (streamNumber strm, strm)
        get = getResponse
        create = openOddStreamWait ctx
    runClient <-
        action $ ClientIO confMySockAddr confPeerSockAddr putR get putB create
    runH2 conf ctx runClient

getResponse :: Stream -> IO Response
getResponse strm = do
    mRsp <- takeMVar $ streamInput strm
    case mRsp of
        Left err -> throwIO err
        Right rsp -> return $ Response rsp

setup :: ClientConfig -> Config -> IO Context
setup ClientConfig{..} conf@Config{..} = do
    let clientInfo = newClientInfo scheme authority
    ctx <-
        newContext
            clientInfo
            conf
            cacheLimit
            connectionWindowSize
            settings
            confTimeoutManager
    exchangeSettings ctx
    return ctx

runH2 :: Config -> Context -> IO a -> IO a
runH2 conf ctx runClient = do
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
    mgr = threadManager ctx
    runReceiver = frameReceiver ctx conf
    runSender = frameSender ctx conf
    runBackgroundThreads = concurrently_ runReceiver runSender

sendRequest
    :: Context
    -> Scheme
    -> Authority
    -> Request
    -> IO Stream
sendRequest ctx@Context{..} scheme auth (Request req) = do
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
            let hdr1, hdr2 :: [Header]
                hdr1
                    | scheme /= "" = (":scheme", scheme) : hdr0
                    | otherwise = hdr0
                hdr2
                    | auth /= "" = (":authority", UTF8.fromString auth) : hdr1
                    | otherwise = hdr1
                req' = req{outObjHeaders = hdr2}
            -- FLOW CONTROL: SETTINGS_MAX_CONCURRENT_STREAMS: send: respecting peer's limit
            (sid, newstrm) <- openOddStreamWait ctx
            sendHeaderBody ctx sid newstrm req'
            return newstrm

sendHeaderBody :: Context -> StreamId -> Stream -> OutObj -> IO ()
sendHeaderBody ctx@Context{..} sid newstrm req = do
    mtbq <- case outObjBody req of
        OutBodyStreaming strmbdy ->
            sendStreaming ctx $ \iface ->
                outBodyUnmask iface $ strmbdy (outBodyPush iface) (outBodyFlush iface)
        OutBodyStreamingUnmask strmbdy ->
            sendStreaming ctx strmbdy
        _ -> return Nothing
    atomically $ do
        sidOK <- readTVar outputQStreamID
        check (sidOK == sid)
        writeTVar outputQStreamID (sid + 2)
    forkManaged threadManager "H2 client send" $
        syncWithSender ctx newstrm (OObj req) mtbq

sendStreaming
    :: Context
    -> (OutBodyIface -> IO ())
    -> IO (Maybe (TBQueue StreamingChunk))
sendStreaming Context{..} strmbdy = do
    tbq <- newTBQueueIO 10 -- fixme: hard coding: 10
    forkManagedUnmask threadManager "H2 client sendStreaming" $ \unmask -> do
        decrementedCounter <- newIORef False
        let decCounterOnce = do
                alreadyDecremented <- atomicModifyIORef decrementedCounter $ \b -> (True, b)
                unless alreadyDecremented $ decCounter threadManager
        let iface =
                OutBodyIface
                    { outBodyUnmask = unmask
                    , outBodyPush = \b -> atomically $ writeTBQueue tbq (StreamingBuilder b Nothing)
                    , outBodyPushFinal = \b -> atomically $ writeTBQueue tbq (StreamingBuilder b (Just decCounterOnce))
                    , outBodyFlush = atomically $ writeTBQueue tbq StreamingFlush
                    }
            finished = atomically $ writeTBQueue tbq $ StreamingFinished decCounterOnce
        incCounter threadManager
        strmbdy iface `finally` finished
    return $ Just tbq

exchangeSettings :: Context -> IO ()
exchangeSettings Context{..} = do
    connRxWS <- rxfBufSize <$> readIORef rxFlow
    let frames = makeNegotiationFrames mySettings connRxWS
        setframe = CFrames Nothing (connectionPreface : frames)
    writeIORef myFirstSettings True
    enqueueControl controlQ setframe

data ClientIO = ClientIO
    { cioMySockAddr :: SockAddr
    , cioPeerSockAddr :: SockAddr
    , cioWriteRequest :: Request -> IO (StreamId, Stream)
    , cioReadResponse :: Stream -> IO Response
    , cioWriteBytes :: ByteString -> IO ()
    , cioCreateStream :: IO (StreamId, Stream)
    }
