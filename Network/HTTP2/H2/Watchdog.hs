{-# LANGUAGE TypeFamilies #-}

-- | What this library records about a connection, and how long it gives
--   the peer to make progress.
--
--   The rules are these, in order:
--
--   1. A write in progress must make progress.
--
--   2. Otherwise, while a request body is waited for on behalf of an
--      application, the peer must make progress.
--
--   3. Otherwise, while an application is running there is no limit: how
--      long a handler takes is the application's business.
--
--   4. Otherwise the connection is idle and the peer must make progress.
--
--   Progress is what is reported with 'rxTick' (the peer) and 'sending'
--   (a write). Under rules 1, 2 and 4, progress restarts the timer, and
--   so does moving from one rule to another.
module Network.HTTP2.H2.Watchdog (
    H2,
    H2Watchdog,
    newH2Watchdog,

    -- * Recording activity
    rxTick,
    sending,
    waitingForPeer,
    runningApp,

    -- * The rest of the watchdog
    timedOutSTM,
    isTimedOut,
    withWatchdog,
) where

import Control.Concurrent.STM (STM, retry)
import qualified Control.Exception as E
import Control.Monad (when)
import System.Watchdog hiding (
    handOver,
    isTimedOut,
    newWatchdog,
    timedOutSTM,
    withWatchdog,
 )
import qualified System.Watchdog as W

-- | One HTTP\/2 connection, as this library drives it.
data H2

-- | The watchdog of one connection.
--
--   With no timeout there is nothing to decide, and recording what the
--   connection does costs nothing: the flag is here, rather than in the
--   context, so that a tick on a hot path is not even a transaction.
data H2Watchdog = H2Watchdog !Bool !(Watchdog H2)

-- | Creating one with a timeout in microseconds. Zero or less means the
--   connection never times out.
newH2Watchdog :: Int -> IO H2Watchdog
newH2Watchdog us = do
    wd <- W.newWatchdog $ H2Context us $ Activity 0 0 0 0 0
    -- Nothing to watch, so no thread watches it.
    when (us <= 0) $ W.handOver wd
    return $ H2Watchdog (us > 0) wd

data Activity = Activity
    { actRx :: Int
    -- ^ Progress of the peer.
    , actTx :: Int
    -- ^ Completed writes.
    , actSending :: Int
    -- ^ Writes in progress.
    , actWaiting :: Int
    -- ^ Request bodies being waited for.
    , actApps :: Int
    -- ^ Applications running.
    }

-- | What the connection must do to stay alive. Two 'Rule's differ when
--   the connection made progress which counts, or moved on.
data Rule
    = Writing Int
    | ReadingForApp Int
    | RunningApp
    | Idling Int
    deriving (Eq)

rule :: Activity -> Rule
rule a
    | actSending a > 0 = Writing $ actTx a
    | actWaiting a > 0 = ReadingForApp $ actRx a
    | actApps a > 0 = RunningApp
    | otherwise = Idling $ actRx a

instance WatchdogFor H2 where
    data ContextFor H2 = H2Context
        { h2Timeout :: Int
        , h2Activity :: Activity
        }

    decide mold new = return $ case mold of
        -- The watchdog starting: the connection has yet to do anything,
        -- and what it is not doing is already on the clock.
        Nothing -> act
        Just old
            | rule (h2Activity old) == r' -> Ignore
            | otherwise -> act
      where
        r' = rule $ h2Activity new
        act
            | RunningApp <- r' = Unlimited
            | otherwise = setTimeout $ h2Timeout new

-- | Succeeding once the connection has timed out.
timedOutSTM :: H2Watchdog -> STM ()
timedOutSTM (H2Watchdog False _) = retry
timedOutSTM (H2Watchdog True wd) = W.timedOutSTM wd

-- | Whether the connection has timed out.
isTimedOut :: H2Watchdog -> IO Bool
isTimedOut (H2Watchdog False _) = return False
isTimedOut (H2Watchdog True wd) = W.isTimedOut wd

-- | Supervising an action with the watchdog thread.
withWatchdog :: H2Watchdog -> IO () -> IO a -> IO a
withWatchdog (H2Watchdog False _) _ action = action
withWatchdog (H2Watchdog True wd) lastResort action =
    W.withWatchdog wd lastResort action

modifyActivity :: H2Watchdog -> (Activity -> Activity) -> IO ()
modifyActivity (H2Watchdog False _) _ = return ()
modifyActivity (H2Watchdog True wd) f =
    update (\c -> c{h2Activity = f $ h2Activity c}) wd

-- | The peer made progress.
rxTick :: H2Watchdog -> IO ()
rxTick wd = modifyActivity wd $ \a -> a{actRx = actRx a + 1}

txTick :: H2Watchdog -> IO ()
txTick wd = modifyActivity wd $ \a -> a{actTx = actTx a + 1}

during
    :: (Activity -> Activity)
    -> (Activity -> Activity)
    -> H2Watchdog
    -> IO a
    -> IO a
during _ _ (H2Watchdog False _) act = act
during begin end wd act =
    E.bracket_ (modifyActivity wd begin) (modifyActivity wd end) act

-- | Running a write.
sending :: H2Watchdog -> IO a -> IO a
sending wd act = do
    r <-
        during
            (\a -> a{actSending = actSending a + 1})
            (\a -> a{actSending = actSending a - 1})
            wd
            act
    txTick wd
    return r

-- | Running a read which waits for the peer on behalf of an application.
waitingForPeer :: H2Watchdog -> IO a -> IO a
waitingForPeer =
    during
        (\a -> a{actWaiting = actWaiting a + 1})
        (\a -> a{actWaiting = actWaiting a - 1})

-- | Running an application.
runningApp :: H2Watchdog -> IO a -> IO a
runningApp =
    during
        (\a -> a{actApps = actApps a + 1})
        (\a -> a{actApps = actApps a - 1})
