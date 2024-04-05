{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Server.Run where

import Control.Concurrent.STM
import Control.Exception
import Imports
import Network.Control (defaultMaxData)
import Network.HTTP.Semantics.Server
import Network.Socket (SockAddr)
import UnliftIO.Async (concurrently_)

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
run sconf@ServerConfig{numberOfWorkers} conf server = do
    ok <- checkPreface conf
    when ok $ do
        (ctx, mgr) <- setup sconf conf
        let wc = fromContext ctx
        setAction mgr $ worker wc mgr server
        replicateM_ numberOfWorkers $ spawnAction mgr
        runH2 conf ctx mgr

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
        (ctx@Context{..}, mgr) <- setup sconf conf
        let ServerInfo{..} = toServerInfo roleInfo
            get = do
                Input strm inObj <- atomically $ readTQueue inputQ
                return (streamNumber strm, strm, Request inObj)
            putR strm (Response outObj) = do
                let out = Output strm outObj OObj Nothing (return ())
                enqueueOutput outputQ out
            putB bs = enqueueControl controlQ $ CFrames Nothing [bs]
        io <- action $ ServerIO confMySockAddr confPeerSockAddr get putR putB
        concurrently_ io $ runH2 conf ctx mgr

checkPreface :: Config -> IO Bool
checkPreface conf@Config{..} = do
    preface <- confReadN connectionPrefaceLength
    if connectionPreface /= preface
        then do
            goaway conf ProtocolError "Preface mismatch"
            return False
        else return True

setup :: ServerConfig -> Config -> IO (Context, Manager)
setup ServerConfig{..} conf@Config{..} = do
    serverInfo <- newServerInfo
    ctx <-
        newContext
            serverInfo
            conf
            0
            connectionWindowSize
            settings
    -- Workers, worker manager and timer manager
    mgr <- start confTimeoutManager
    return (ctx, mgr)

runH2 :: Config -> Context -> Manager -> IO ()
runH2 conf ctx mgr = do
    let runReceiver = frameReceiver ctx conf
        runSender = frameSender ctx conf mgr
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
