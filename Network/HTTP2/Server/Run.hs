{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Server.Run where

import Control.Concurrent.STM
import Imports
import Network.Control (defaultMaxData)
import Network.HTTP.Semantics.IO
import Network.HTTP.Semantics.Server
import Network.HTTP.Semantics.Server.Internal
import Network.Socket (SockAddr)
import UnliftIO.Async (concurrently_)

import Network.HTTP2.Frame
import Network.HTTP2.H2
import Network.HTTP2.Server.Worker

-- | Server configuration
data ServerConfig = ServerConfig
    { connectionWindowSize :: WindowSize
    -- ^ The window size of incoming streams
    , settings :: Settings
    -- ^ Settings
    }
    deriving (Eq, Show)

-- | The default server config.
--
-- >>> defaultServerConfig
-- ServerConfig {connectionWindowSize = 16777216, settings = Settings {headerTableSize = 4096, enablePush = True, maxConcurrentStreams = Just 64, initialWindowSize = 262144, maxFrameSize = 16384, maxHeaderListSize = Nothing, pingRateLimit = 10, emptyFrameRateLimit = 4, settingsRateLimit = 4, rstRateLimit = 4}}
defaultServerConfig :: ServerConfig
defaultServerConfig =
    ServerConfig
        { connectionWindowSize = defaultMaxData
        , settings = defaultSettings
        }

----------------------------------------------------------------

-- | Running HTTP/2 server.
run :: ServerConfig -> Config -> Server -> IO ()
run sconf conf server = do
    ok <- checkPreface conf
    when ok $ do
        let lnch ctx strm inpObj = do
                let label = "H2 worker for stream " ++ show (streamNumber strm)
                forkManaged (threadManager ctx) label $
                    worker conf server ctx strm inpObj
        ctx <- setup sconf conf lnch
        runH2 conf ctx

----------------------------------------------------------------

data ServerIO a = ServerIO
    { sioMySockAddr :: SockAddr
    , sioPeerSockAddr :: SockAddr
    , sioReadRequest :: IO (a, Request)
    , sioWriteResponse :: a -> Response -> IO ()
    -- ^ 'Response' MUST be created with 'responseBuilder'.
    -- Others are not supported.
    }

-- | Launching a receiver and a sender without workers.
-- Any frames can be sent with `sioWriteBytes`.
runIO
    :: ServerConfig
    -> Config
    -> (ServerIO Stream -> IO (IO ()))
    -> IO ()
runIO sconf conf@Config{..} action = do
    ok <- checkPreface conf
    when ok $ do
        inpQ <- newTQueueIO
        let lnch _ strm inpObj = atomically $ writeTQueue inpQ (strm, inpObj)
        ctx@Context{..} <- setup sconf conf lnch
        let get = do
                (strm, inpObj) <- atomically $ readTQueue inpQ
                return (strm, Request inpObj)
            putR strm (Response OutObj{..}) = do
                case outObjBody of
                    OutBodyBuilder builder -> do
                        let next = fillBuilderBodyGetNext builder
                            sync _ = return True
                            out = OHeader outObjHeaders (Just next) outObjTrailers
                        enqueueOutput outputQ $ Output strm out sync
                    _ -> error "Response other than OutBodyBuilder is not supported"
            serverIO =
                ServerIO
                    { sioMySockAddr = confMySockAddr
                    , sioPeerSockAddr = confPeerSockAddr
                    , sioReadRequest = get
                    , sioWriteResponse = putR
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
    stopAfter mgr runBackgroundThreads $ \res ->
        closeAllStreams (oddStreamTable ctx) (evenStreamTable ctx) res

-- connClose must not be called here since Run:fork calls it
goaway :: Config -> ErrorCode -> ByteString -> IO ()
goaway Config{..} etype debugmsg = confSendAll bytestream
  where
    bytestream = goawayFrame 0 etype debugmsg
