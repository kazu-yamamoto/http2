{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Server.Run where

import Control.Concurrent.STM
import Control.Exception
import Imports
import Network.HTTP2.Arch
import Network.HTTP2.Frame
import Network.HTTP2.Server.Types
import Network.HTTP2.Server.Worker
import Network.Socket (SockAddr)
import UnliftIO.Async (concurrently_)

-- | Server configuration
data ServerConfig = ServerConfig
    { numberOfworkers :: Int
    -- ^ The number of workers
    , maxConcurrency :: Int
    }

-- | The number of workers is 3.
-- This was carefully chosen based on a lot of benchmarks.
-- If it is 1, we cannot avoid head-of-line blocking.
-- If it is large, huge memory is consumed and many
-- context switches happen.
defaultServerConfig :: ServerConfig
defaultServerConfig =
    ServerConfig
        { numberOfworkers = 3
        , maxConcurrency = recommendedConcurrency
        }

----------------------------------------------------------------

-- | Running HTTP/2 server.
run :: Config -> Server -> IO ()
run = run' defaultServerConfig

run' :: ServerConfig -> Config -> Server -> IO ()
run' sconf@ServerConfig{numberOfworkers} conf server = do
    ok <- checkPreface conf
    when ok $ do
        (ctx, mgr) <- setup sconf conf
        let wc = fromContext ctx
        setAction mgr $ worker wc mgr server
        replicateM_ numberOfworkers $ spawnAction mgr
        runArch conf ctx mgr

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
        concurrently_ io $ runArch conf ctx mgr

checkPreface :: Config -> IO Bool
checkPreface conf@Config{..} = do
    preface <- confReadN connectionPrefaceLength
    if connectionPreface /= preface
        then do
            goaway conf ProtocolError "Preface mismatch"
            return False
        else return True

setup :: ServerConfig -> Config -> IO (Context, Manager)
setup _ Config{..} = do
    serverInfo <- newServerInfo
    ctx <- newContext serverInfo 0 confBufferSize confMySockAddr confPeerSockAddr
    -- Workers, worker manager and timer manager
    mgr <- start confTimeoutManager
    return (ctx, mgr)

runArch :: Config -> Context -> Manager -> IO ()
runArch conf ctx mgr = do
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
