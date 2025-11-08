{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Client.Run where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import qualified Data.ByteString.UTF8 as UTF8
import Data.IORef
import Data.IP (IPv6)
import Network.Control (RxFlow (..), defaultMaxData)
import Network.HTTP.Semantics.Client
import Network.HTTP.Semantics.Client.Internal
import Network.HTTP.Semantics.IO
import Network.Socket (SockAddr)
import qualified System.ThreadManager as T
import Text.Read (readMaybe)

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
-- ClientConfig {scheme = "http", authority = "localhost", cacheLimit = 64, connectionWindowSize = 16777216, settings = Settings {headerTableSize = 4096, enablePush = True, maxConcurrentStreams = Just 64, initialWindowSize = 262144, maxFrameSize = 16384, maxHeaderListSize = Nothing, pingRateLimit = 10, emptyFrameRateLimit = 4, settingsRateLimit = 4, rstRateLimit = 4}}
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
            , auxSendPing =
                sendPing
                    ctx
                    False
                    "Haskell!" -- 8 bytes
            }
    clientCore ctx req processResponse = do
        (strm, moutobj) <- makeStream ctx scheme authority req
        case moutobj of
            Nothing -> return ()
            Just outobj -> sendRequest conf ctx strm outobj False
        rsp <- getResponse strm
        x <- processResponse rsp
        adjustRxWindow ctx strm
        return x
    runClient ctx = client (clientCore ctx) $ aux ctx

-- | Launching a receiver and a sender.
runIO :: ClientConfig -> Config -> (ClientIO -> IO (IO a)) -> IO a
runIO cconf@ClientConfig{..} conf@Config{..} action = do
    ctx@Context{..} <- setup cconf conf
    let putB bs = enqueueControl controlQ $ CFrames Nothing [bs]
        putR req = do
            (strm, moutobj) <- makeStream ctx scheme authority req
            case moutobj of
                Nothing -> return ()
                Just outobj -> sendRequest conf ctx strm outobj True
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
    T.stopAfter mgr (try runAll >>= closureClient conf ctx) $ \res ->
        closeAllStreams (oddStreamTable ctx) (evenStreamTable ctx) res
  where
    mgr = threadManager ctx
    runReceiver = frameReceiver ctx conf
    runSender = frameSender ctx conf
    runBackgroundThreads = do
        labelMe "H2 runBackgroundThreads"
        concurrently_ runReceiver runSender
    runAll = do
        -- "runBackgroundThreads" may throw an exception which
        -- will be caught by "try" above
        er <- race runBackgroundThreads runClient
        case er of
            Left () -> undefined -- never reached
            Right r -> return r

makeStream
    :: Context
    -> Scheme
    -> Authority
    -> Request
    -> IO (Stream, Maybe OutObj)
makeStream ctx@Context{..} scheme auth (Request req) = do
    -- Checking push promises
    let hdr0 = outObjHeaders req
        method = fromMaybe (error "makeStream:method") $ lookup ":method" hdr0
        path = fromMaybe (error "makeStream:path") $ lookup ":path" hdr0
    mstrm0 <- lookupEvenCache evenStreamTable method path
    case mstrm0 of
        Just strm0 -> do
            deleteEvenCache evenStreamTable method path
            return (strm0, Nothing)
        Nothing -> do
            -- Arch/Sender is originally implemented for servers where
            -- the ordering of responses can be out-of-order.
            -- But for clients, the ordering must be maintained.
            -- To implement this, 'outputQStreamID' is used.
            let isIPv6 = isJust (readMaybe auth :: Maybe IPv6)
                auth'
                    | isIPv6 = "[" <> UTF8.fromString auth <> "]"
                    | otherwise = UTF8.fromString auth
            let hdr1, hdr2 :: [Header]
                hdr1
                    | scheme /= "" = (":scheme", scheme) : hdr0
                    | otherwise = hdr0
                hdr2
                    | auth /= "" = (":authority", auth') : hdr1
                    | otherwise = hdr1
                req' = req{outObjHeaders = hdr2}
            -- FLOW CONTROL: SETTINGS_MAX_CONCURRENT_STREAMS: send: respecting peer's limit
            (_sid, newstrm) <- openOddStreamWait ctx
            return (newstrm, Just req')

sendRequest :: Config -> Context -> Stream -> OutObj -> Bool -> IO ()
sendRequest Config{..} ctx@Context{..} strm OutObj{..} io = do
    let sid = streamNumber strm
    (mnext, mtbq) <- case outObjBody of
        OutBodyNone -> return (Nothing, Nothing)
        OutBodyFile (FileSpec path fileoff bytecount) -> do
            (pread, sentinel) <- confPositionReadMaker path
            let next = fillFileBodyGetNext pread fileoff bytecount sentinel
            return (Just next, Nothing)
        OutBodyBuilder builder -> do
            let next = fillBuilderBodyGetNext builder
            return (Just next, Nothing)
        OutBodyStreaming strmbdy -> do
            q <- sendStreaming ctx strm $ \iface ->
                outBodyUnmask iface $ strmbdy (outBodyPush iface) (outBodyFlush iface)
            let next = nextForStreaming q
            return (Just next, Just q)
        OutBodyStreamingIface strmbdy -> do
            q <- sendStreaming ctx strm strmbdy
            let next = nextForStreaming q
            return (Just next, Just q)
    let ot = OHeader outObjHeaders mnext outObjTrailers
    if io
        then do
            let out = makeOutputIO ctx strm ot
            pushOutput sid out
        else do
            (pop, out) <- makeOutput strm ot
            pushOutput sid out
            lc <- newLoopCheck strm mtbq
            T.forkManaged threadManager label $ syncWithSender' ctx pop lc
  where
    label = "H2 request sender for stream " ++ show (streamNumber strm)
    pushOutput sid out = atomically $ do
        sidOK <- readTVar outputQStreamID
        check (sidOK == sid)
        writeTVar outputQStreamID (sid + 2)
        enqueueOutputSTM outputQ out

sendStreaming
    :: Context
    -> Stream
    -> (OutBodyIface -> IO ())
    -> IO (TBQueue StreamingChunk)
sendStreaming Context{..} strm strmbdy = do
    tbq <- newTBQueueIO 10 -- fixme: hard coding: 10
    T.forkManagedUnmask threadManager label $ \unmask ->
        withOutBodyIface tbq unmask strmbdy
    return tbq
  where
    label = "H2 request streaming sender for stream " ++ show (streamNumber strm)

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
