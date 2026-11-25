{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Server.Run where

import Control.Concurrent (myThreadId)
import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.IntMap.Strict as IntMap
import qualified System.ThreadManager as T
import Imports
import Network.Control (defaultMaxData)
import Network.HTTP.Semantics.IO
import Network.HTTP.Semantics.Server
import Network.HTTP.Semantics.Server.Internal
import Network.HTTP2.H2.Watchdog
import Network.Socket (SockAddr)

import Network.HTTP2.Frame
import Network.HTTP2.H2
import Network.HTTP2.Server.Worker

-- | Server configuration
data ServerConfig = ServerConfig
    { numberOfWorkers :: Int
    -- ^ Deprecated field.
    , connectionWindowSize :: WindowSize
    -- ^ The window size of incoming streams
    , settings :: Settings
    -- ^ Settings
    }
    deriving (Eq, Show)

{-# DEPRECATED numberOfWorkers "No effect anymore" #-}

-- | The default server config.
--
-- >>> defaultServerConfig
-- ServerConfig {numberOfWorkers = 8, connectionWindowSize = 16777216, settings = Settings {headerTableSize = 4096, enablePush = True, maxConcurrentStreams = Just 64, initialWindowSize = 262144, maxFrameSize = 16384, maxHeaderListSize = Just 65536, pingRateLimit = 10, emptyFrameRateLimit = 4, settingsRateLimit = 4, rstRateLimit = 4}}
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
run = runMaybe Nothing

-- | Like 'run', but the connection goes away once the 'STM' action fires:
--   GOAWAY(NO_ERROR) at once, streams above it refused, and the
--   connection closed when those below it are done.
--
--   @since 5.4.13
runWith :: STM () -> ServerConfig -> Config -> Server -> IO ()
runWith = runMaybe . Just

-- Nothing for 'run': no thread waits on an action which never fires.
runMaybe :: Maybe (STM ()) -> ServerConfig -> Config -> Server -> IO ()
runMaybe mGoAway sconf conf server = supervising conf $ \wd -> do
    ok <- checkPreface conf wd mGoAway
    when ok $ do
        let lnch = runServer conf server
        ctx <- setup sconf conf wd lnch Nothing
        runH2 conf ctx mGoAway Nothing

-- | Running an action with the watchdog of this connection.
--
--   The preface is the first thing read from the peer, and a peer which
--   does not send it is a peer this connection waits for.  So the
--   watchdog starts here, before 'checkPreface', rather than in 'runH2'.
--
--   The connection finishes by itself once it times out, when 'Config'
--   can give up on its reads and writes.  Throwing to it is the last
--   resort, for one whose 'confReadN' or 'confSendAll' cannot give up.
supervising :: Config -> (H2Watchdog -> IO a) -> IO a
supervising Config{..} action = do
    wd <- newH2Watchdog confTimeout
    tid <- myThreadId
    let lastResort
            | isJust confReadNWith && isJust confSendAllWith = return ()
            | otherwise = E.throwTo tid $ LastResort ConnectionIsTimeout
    lastResortAsError $ withWatchdog wd lastResort $ action wd

----------------------------------------------------------------

data ServerIO a = ServerIO
    { sioMySockAddr :: SockAddr
    , sioPeerSockAddr :: SockAddr
    , sioReadRequest :: IO (a, Request)
    , sioWriteResponse :: a -> Response -> IO ()
    -- ^ 'Response' MUST be created with 'responseBuilder'.
    -- Others are not supported.
    , sioDone :: IO ()
    }

-- | Launching a receiver and a sender without workers.
-- Any frames can be sent with `sioWriteBytes`.
runIO
    :: ServerConfig
    -> Config
    -> (ServerIO Stream -> IO (IO ()))
    -> IO ()
runIO = runIOMaybe Nothing

-- | Like 'runIO', but the connection goes away once the 'STM' action
--   fires, as with 'runWith'.
--
--   @since 5.4.13
runIOWith
    :: STM ()
    -> ServerConfig
    -> Config
    -> (ServerIO Stream -> IO (IO ()))
    -> IO ()
runIOWith = runIOMaybe . Just

runIOMaybe
    :: Maybe (STM ())
    -> ServerConfig
    -> Config
    -> (ServerIO Stream -> IO (IO ()))
    -> IO ()
runIOMaybe mGoAway sconf conf@Config{..} action = supervising conf $ \wd -> do
    ok <- checkPreface conf wd mGoAway
    when ok $ do
        inpQ <- newTQueueIO
        -- Set by 'sioDone': first 'closing', so that nothing more is taken,
        -- then 'done', once the refusals are queued for the sender, which
        -- ends on 'done'.
        closing <- newTVarIO False
        done <- newTVarIO False
        -- Requests handed over and not answered yet, each an application
        -- running for the watchdog, as a handler of 'run' is.  One whose
        -- stream closed unanswered is let go when next looked at.
        pending <- newTVarIO IntMap.empty
        let refuse ctx strm =
                resetStreamWith RefusedStream ctx strm $
                    E.toException $
                        StreamErrorIsSent RefusedStream (streamNumber strm) "runIO is done"
            -- Once the application is done, what it would never read is
            -- refused, so that the client can retry it elsewhere.
            lnch ctx strm inpObj = do
                appStarted wd
                taken <- atomically $ do
                    c <- readTVar closing
                    unless c $ do
                        writeTQueue inpQ (strm, inpObj)
                        modifyTVar' pending $ IntMap.insert (streamNumber strm) strm
                    return $ not c
                unless taken $ do
                    appEnded wd
                    refuse ctx strm
            -- Letting go of the streams answered, or closed meanwhile.
            letGo strm = do
                n <- atomically $ do
                    ps <- IntMap.delete (streamNumber strm) <$> readTVar pending
                    open <- filterM (fmap (not . isClosed) . readTVar . streamState . snd) $ IntMap.toList ps
                    old <- IntMap.size <$> readTVar pending
                    writeTVar pending $ IntMap.fromList open
                    return $ old - length open
                replicateM_ n $ appEnded wd
        ctx <- setup sconf conf wd lnch $ Just $ readTVar done
        let get = do
                (strm, inpObj) <- atomically $ readTQueue inpQ
                let body = waitingForPeer wd $ inpObjBody inpObj
                return (strm, Request inpObj{inpObjBody = body})
            finish = do
                (left, ps) <- atomically $ do
                    writeTVar closing True
                    left <- flushTQueue inpQ
                    ps <- readTVar pending
                    writeTVar pending IntMap.empty
                    return (left, ps)
                replicateM_ (IntMap.size ps) $ appEnded wd
                forM_ left $ refuse ctx . fst
                atomically $ writeTVar done True
            putR strm (Response OutObj{..}) = do
                letGo strm
                case outObjBody of
                    OutBodyBuilder builder -> do
                        let next = fillBuilderBodyGetNext builder
                            otyp = OHeader outObjHeaders (Just next) outObjTrailers
                        enqueueOutputSIO ctx strm otyp
                    _ -> error "Response other than OutBodyBuilder is not supported"
            serverIO =
                ServerIO
                    { sioMySockAddr = confMySockAddr
                    , sioPeerSockAddr = confPeerSockAddr
                    , sioReadRequest = get
                    , sioWriteResponse = putR
                    , sioDone = finish
                    }
        io <- action serverIO
        runH2 conf ctx mGoAway $ Just io

-- | Reading the preface.  A peer which never sends it is given up on like
--   any other, and is not waited for once the server is going away: the
--   connection then ends normally, as one unused does.
checkPreface :: Config -> H2Watchdog -> Maybe (STM ()) -> IO Bool
checkPreface conf wd mGoAway = do
    r <- readNGivingUp conf connectionPrefaceLength $
        timedOutSTM wd `orElse` fromMaybe retry mGoAway
    case r of
        Left () -> do
            timedOut <- isTimedOut wd
            if timedOut then E.throwIO ConnectionIsTimeout else return False
        Right preface
            | connectionPreface /= preface -> do
                goaway conf ProtocolError "Preface mismatch"
                return False
            | otherwise -> return True

setup
    :: ServerConfig
    -> Config
    -> H2Watchdog
    -> Launch
    -> Maybe (STM Bool)
    -> IO Context
setup ServerConfig{..} conf wd lnch mIsDone = do
    let serverInfo = newServerInfo lnch
    newContext
        serverInfo
        conf
        wd
        0
        connectionWindowSize
        settings
        mIsDone

runH2 :: Config -> Context -> Maybe (STM ()) -> Maybe (IO ()) -> IO ()
runH2 conf ctx mGoAway mIO = do
    -- Supervised by 'supervising', which 'run' and 'runIO' wrap this in.
    let runReceiver = frameReceiver ctx conf
        runSender = frameSender ctx conf
        -- Two GOAWAYs, as RFC 9113 section 6.8 has it: the first, with
        -- the largest identifier, stops the client opening streams; the
        -- second fixes the last one taken once a PING has gone round, so
        -- that the streams it opened meanwhile are not refused.  The
        -- receiver is stopped as it is after the peer's GOAWAY, once the
        -- streams below ours are done; it reads until then.
        sendGoAway :: STM () -> T.Managed a -> IO ()
        sendGoAway goAway ar = do
            told <- atomically $ (True <$ goAway) `orElse` (False <$ stopping)
            when told $ goingAwayTold ar
        goingAwayTold ar = do
            enqueueControl (controlQ ctx) $
                CFrames
                    Nothing
                    [ goawayFrame maxStreamId NoError "going away"
                    , pingFrame False goAwayPing
                    ]
            roundTrip <- registerDelay 1000000
            atomically $
                (readTVar (pingAcked ctx) >>= check . (== goAwayPing))
                    `orElse` (readTVar roundTrip >>= check)
            mlast <- atomically $ goingAwayMine ctx
            forM_ mlast $ \lastSid ->
                enqueueControl (controlQ ctx) $
                    CFrames Nothing [goawayFrame lastSid NoError "going away"]
            isDrained <- atomically $ (True <$ drainedMine ctx) `orElse` (False <$ stopping)
            when isDrained $ stopReceiver ctx $ T.managedThreadId ar
        stopping = T.waitStoppingSTM $ threadManager ctx
        -- Library threads of the context's manager, told to stop with a
        -- grace by 'stopAfterAndWait'.  They were async's, which no manager
        -- saw, and were killed at once on leaving.
        runBackgroundThreads = do
            -- The application is user code: killed when the connection
            -- ends, and its failure ends the connection.
            mApp <- forM mIO $ T.forkManagedWithResult (appThreadManager ctx) "H2 runIO application"
            ar <- forkCore ctx "H2 receiver" runReceiver
            forM_ mGoAway $ \goAway ->
                forkCore ctx "H2 going away" $ sendGoAway goAway ar
            as <- forkCore ctx "H2 sender" runSender
            let appFailed = case mApp of
                    Nothing -> retry
                    Just app -> T.waitManagedSTM app >>= either return (const retry)
            ra <-
                (`E.onException` abandonConnection ctx as) $
                    atomically $
                        (Right <$> T.waitEitherManagedSTM ar as) `orElse` (Left <$> appFailed)
            r <- case ra of
                Right r -> return r
                -- The sender is stopped first, so that GOAWAY is written
                -- alone.
                Left err -> do
                    abandonConnection ctx as
                    goawayWith conf ctx InternalError "application failed"
                    E.throwIO err
            e <- case r of
                -- The receiver is done; the sender finishes once it
                -- has flushed what is queued.
                Left _ -> T.waitManaged as
                -- The sender finished first.  Either the receiver is
                -- done too and not yet seen to be, and this is its
                -- error, or the sender failed: nothing more would go
                -- out, and leaving the receiver to run on left the
                -- connection open and silent, with no GOAWAY.  Both
                -- are closed with it: the receiver is stopped
                -- rather than cancelled when its reads can give up.
                Right e -> do
                    stopReceiver ctx $ T.managedThreadId ar
                    late <- registerDelay 1000000
                    atomically $
                        void (T.waitManagedSTM ar) `orElse` (readTVar late >>= check)
                    return e
            endServer conf ctx e
    stopAfterAndWait ctx runBackgroundThreads $ \res ->
        closeAllStreams (oddStreamTable ctx) (evenStreamTable ctx) res

-- | The payload of the PING sent with the first GOAWAY.
goAwayPing :: ByteString
goAwayPing = "goaway\0\0"

-- | The largest stream identifier.
maxStreamId :: StreamId
maxStreamId = 2147483647

-- connClose must not be called here since Run:fork calls it
goaway :: Config -> ErrorCode -> ByteString -> IO ()
goaway conf etype debugmsg = sendGoaway conf bytestream
  where
    bytestream = goawayFrame 0 etype debugmsg
