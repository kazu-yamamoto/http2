{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Server.Run where

import Control.Concurrent.STM
import Imports
import Network.Control (defaultMaxData)
import Network.HTTP.Semantics.Server
import Network.HTTP.Semantics.Server.Internal
import Network.Socket (SockAddr)
import UnliftIO.Async (concurrently_)
import UnliftIO.Exception

import Network.HTTP2.Frame
import Network.HTTP2.H2
import Network.HTTP2.Server.Worker

-- | Server configuration
data ServerConfig = ServerConfig
    { numberOfWorkers :: Int
    -- ^ The number of workers
    , connectionWindowSize :: WindowSize
    -- ^ The window size of incoming streams
    , settings :: Settings
    -- ^ Settings
    }
    deriving (Eq, Show)

-- | The default server config.
--
-- >>> defaultServerConfig
-- ServerConfig {numberOfWorkers = 8, connectionWindowSize = 1048576, settings = Settings {headerTableSize = 4096, enablePush = True, maxConcurrentStreams = Just 64, initialWindowSize = 262144, maxFrameSize = 16384, maxHeaderListSize = Nothing, pingRateLimit = 10}}
defaultServerConfig :: ServerConfig
defaultServerConfig =
    ServerConfig
        { numberOfWorkers = 8
        , connectionWindowSize = defaultMaxData
        , settings = defaultSettings
        }

----------------------------------------------------------------

-- | Running HTTP/2 server.
run :: ServerConfig -> Config -> Server -> IO ()
run sconf conf server = do
    ok <- checkPreface conf
    when ok $ do
        let lnch ctx strm inpObj = do
                let label = "Worker for stream " ++ show (streamNumber strm)
                    wc = fromContext ctx
                forkManaged (threadManager ctx) label $
                    worker wc server ctx strm inpObj
        ctx <- setup sconf conf lnch
        runH2 conf ctx

----------------------------------------------------------------

data ServerIO = ServerIO
    { sioMySockAddr :: SockAddr
    , sioPeerSockAddr :: SockAddr
    , sioReadRequest :: IO (StreamId, Stream, Request)
    , sioWriteResponse :: Stream -> Response -> IO ()
    , sioWriteBytes :: ByteString -> IO ()
    }

-- | Launching a receiver and a sender without workers.
-- Any frames can be sent with `sioWriteBytes`.
runIO
    :: ServerConfig
    -> Config
    -> (ServerIO -> IO (IO ()))
    -> IO ()
runIO sconf conf@Config{..} action = do
    ok <- checkPreface conf
    when ok $ do
        inpQ <- newTQueueIO
        let lnch _ strm inpObj = atomically $ writeTQueue inpQ (strm, inpObj)
        ctx@Context{..} <- setup sconf conf lnch
        let get = do
                (strm, inpObj) <- atomically $ readTQueue inpQ
                return (streamNumber strm, strm, Request inpObj)
            putR strm (Response outObj) = do
                let out = Output strm outObj OObj Nothing (return ())
                enqueueOutput outputQ out
            putB bs = enqueueControl controlQ $ CFrames Nothing [bs]
            serverIO =
                ServerIO
                    { sioMySockAddr = confMySockAddr
                    , sioPeerSockAddr = confPeerSockAddr
                    , sioReadRequest = get
                    , sioWriteResponse = putR
                    , sioWriteBytes = putB
                    }
        io <- action serverIO
        concurrently_ io $ runH2 conf ctx

checkPreface :: Config -> IO Bool
checkPreface conf@Config{..} = do
    preface <- confReadN connectionPrefaceLength
    if connectionPreface /= preface
        then do
            goaway conf ProtocolError "Preface mismatch"
            return False
        else return True

setup :: ServerConfig -> Config -> Launch -> IO Context
setup ServerConfig{..} conf@Config{..} lnch = do
    let serverInfo = newServerInfo lnch
    newContext
        serverInfo
        conf
        0
        connectionWindowSize
        settings
        confTimeoutManager

runH2 :: Config -> Context -> IO ()
runH2 conf ctx = do
    let mgr = threadManager ctx
        runReceiver = frameReceiver ctx conf
        runSender = frameSender ctx conf
        runBackgroundThreads = concurrently_ runReceiver runSender
    stopAfter mgr runBackgroundThreads $ \res -> do
        closeAllStreams (oddStreamTable ctx) (evenStreamTable ctx) $
            either Just (const Nothing) res
        case res of
            Left err ->
                throwIO err
            Right x ->
                return x

-- connClose must not be called here since Run:fork calls it
goaway :: Config -> ErrorCode -> ByteString -> IO ()
goaway Config{..} etype debugmsg = confSendAll bytestream
  where
    bytestream = goawayFrame 0 etype debugmsg
