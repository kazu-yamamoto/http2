-- | One timeout supervisor per connection.
--
-- The connection records what it is doing in 'TVar's and a separate
-- thread, the watchdog, decides whether it has been doing it for too
-- long. When it has, the watchdog does not throw anything: it writes
-- 'True' to a 'TVar', and the sender, which waits for its queues with
-- STM, composes that 'TVar' with them and finishes by itself. Then the
-- connection is closed with GOAWAY as usual.
--
-- What counts as "too long" is decided from the state alone:
--
-- 1. A write in progress must make progress.
--
-- 2. Otherwise, when a stream waits for its request body, the peer
--    must make progress.
--
-- 3. Otherwise, while a server application is running there is no
--    limit: how long a handler takes is the application's business.
--
-- 4. Otherwise the connection is idle and the peer must make progress.
--
-- Progress is exactly this:
--
-- * The peer makes progress when the header of a frame arrives, which
--   also means that the payload of the previous frame was read in full.
--   Any frame counts, on any stream, including PING, SETTINGS and
--   WINDOW_UPDATE. So a peer can keep an idle connection alive with
--   PINGs, as far as 'pingRateLimit' allows. A frame which trickles in
--   counts only once its header is complete.
--
-- * A write makes progress when a call to 'confSendAll' returns. A call
--   which is blocked counts for nothing, however many bytes the kernel
--   took.
--
-- Under rules 1, 2 and 4, progress restarts the timer: the connection
-- times out when the timeout passes without any. Moving from one rule
-- to another, such as a write starting or an application finishing, also
-- restarts it. The watchdog looks at the connection at most once a
-- second, so progress may be noticed up to a second late, and a timeout
-- may fire up to a second late accordingly.
--
-- The timer is the one 'T.Handle' of the connection, taken from
-- 'confTimeoutManager' and touched by the watchdog thread only. So, with
-- 'T.defaultManager', nothing ever times out.
--
-- When 'confReadNTimeout' is 'True', the caller supervises the connection
-- (Warp does) and no watchdog runs.
module Network.HTTP2.H2.Watchdog (
    Watchdog,
    newWatchdog,

    -- * Recording activity
    rxTick,
    sending,
    waitingForPeer,
    runningApp,

    -- * Observing a timeout
    timedOutSTM,

    -- * Supervision
    withWatchdog,
) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified System.TimeManager as T

import Imports
import Network.HTTP2.H2.Types (HTTP2Error (..))

----------------------------------------------------------------

data Activity = Activity
    { actRx :: Int
    -- ^ Frames received.
    , actTx :: Int
    -- ^ Writes completed.
    , actSending :: Int
    -- ^ Writes in progress.
    , actWaiting :: Int
    -- ^ Streams waiting for their request body.
    , actApps :: Int
    -- ^ Server applications running.
    }
    deriving (Eq)

-- | The supervised state of one connection.
data Watchdog = Watchdog
    { wdEnabled :: Bool
    , wdActivity :: TVar Activity
    , wdTimedOut :: TVar Bool
    }

-- | Creating the state. No thread is started until 'withWatchdog'.
--   When disabled, recording activity costs nothing.
newWatchdog :: Bool -> IO Watchdog
newWatchdog enabled =
    Watchdog enabled <$> newTVarIO (Activity 0 0 0 0 0) <*> newTVarIO False

modifyActivity :: Watchdog -> (Activity -> Activity) -> IO ()
modifyActivity wd f =
    when (wdEnabled wd) $ atomically $ modifyTVar' (wdActivity wd) f

-- | The peer made progress.
rxTick :: Watchdog -> IO ()
rxTick wd = modifyActivity wd $ \a -> a{actRx = actRx a + 1}

-- | Running a write.
sending :: Watchdog -> IO a -> IO a
sending wd act
    | wdEnabled wd = do
        r <-
            E.bracket_
                (modifyActivity wd $ \a -> a{actSending = actSending a + 1})
                (modifyActivity wd $ \a -> a{actSending = actSending a - 1})
                act
        modifyActivity wd $ \a -> a{actTx = actTx a + 1}
        return r
    | otherwise = act

-- | Running a read of a request body.
waitingForPeer :: Watchdog -> IO a -> IO a
waitingForPeer wd
    | wdEnabled wd =
        E.bracket_
            (modifyActivity wd $ \a -> a{actWaiting = actWaiting a + 1})
            (modifyActivity wd $ \a -> a{actWaiting = actWaiting a - 1})
    | otherwise = id

-- | Running a server application.
runningApp :: Watchdog -> IO a -> IO a
runningApp wd
    | wdEnabled wd =
        E.bracket_
            (modifyActivity wd $ \a -> a{actApps = actApps a + 1})
            (modifyActivity wd $ \a -> a{actApps = actApps a - 1})
    | otherwise = id

----------------------------------------------------------------

-- | Succeeding once the watchdog has decided this connection timed out.
--   Retrying otherwise.
timedOutSTM :: Watchdog -> STM ()
timedOutSTM wd = readTVar (wdTimedOut wd) >>= check

----------------------------------------------------------------

-- | What the connection must do to stay alive. Two 'Rule's differ
--   when the connection made progress which counts, or moved on.
data Rule
    = Writing Int
    | ReadingForApp Int
    | RunningApp
    | Idle Int
    deriving (Eq)

rule :: Activity -> Rule
rule a
    | actSending a > 0 = Writing $ actTx a
    | actWaiting a > 0 = ReadingForApp $ actRx a
    | actApps a > 0 = RunningApp
    | otherwise = Idle $ actRx a

data Event = Done | Moved Rule | Expired

-- | Supervising an action with the watchdog thread, if enabled.
--
--   If the connection does not finish within another timeout after it
--   was told to, 'ConnectionIsTimeout' is thrown to the thread running
--   the action, as the last resort.
withWatchdog :: T.Manager -> Watchdog -> IO a -> IO a
withWatchdog mgr wd action
    | not (wdEnabled wd) = action
    | otherwise = do
        tid <- myThreadId
        expired <- newTVarIO False
        T.withHandle mgr (atomically $ writeTVar expired True) $ \th -> do
            done <- newTVarIO False
            finished <- newTVarIO False
            void $ forkIO $ do
                labelMe "H2 watchdog"
                watchdog th expired done tid wd
                    `E.finally` atomically (writeTVar finished True)
            action `E.finally` do
                atomically $ writeTVar done True
                atomically $ readTVar finished >>= check

watchdog
    :: T.Handle -> TVar Bool -> TVar Bool -> ThreadId -> Watchdog -> IO ()
watchdog th expired done tid wd = do
    r0 <- atomically currentRule
    arm r0
    loop r0
  where
    currentRule = rule <$> readTVar (wdActivity wd)

    -- Restarting the timer, or stopping it. 'T.pause' first, since a
    -- timer which has fired cannot be extended.
    arm RunningApp = T.pause th
    arm _ = restart

    restart = do
        T.pause th
        T.resume th
        atomically $ writeTVar expired False

    -- If the connection moved and the timer expired at the same time,
    -- it moved: a connection which turns out to be alive is not killed.
    wait r0 =
        atomically $
            (readTVar done >>= check >> return Done)
                <|> (currentRule >>= \r -> check (r /= r0) >> return (Moved r))
                <|> (readTVar expired >>= check >> return Expired)

    loop r0 = do
        ev <- wait r0
        case ev of
            Done -> return ()
            Moved r -> do
                arm r
                -- However busy the connection is, the watchdog wakes up
                -- once per this interval at most.
                ok <- sleep 1000000
                when ok $ loop r
            Expired -> do
                atomically $ writeTVar (wdTimedOut wd) True
                restart
                ev' <-
                    atomically $
                        (readTVar done >>= check >> return Done)
                            <|> (readTVar expired >>= check >> return Expired)
                case ev' of
                    Done -> return ()
                    _ -> E.throwTo tid ConnectionIsTimeout

    -- Returning 'False' when done.
    sleep us = do
        var <- registerDelay us
        atomically $
            (readTVar done >>= check >> return False)
                <|> (readTVar var >>= check >> return True)
