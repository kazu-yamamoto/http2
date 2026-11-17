{-# LANGUAGE TypeFamilies #-}

-- | What this library records about a connection, and how long it gives
--   the peer to make progress.
--
--   The rules are these, in order:
--
--   1. A write in progress must make progress.
--
--   2. Otherwise, while a request body is waited for on behalf of an
--      application, or a response waits for a window the peer has shut,
--      the peer must make progress.
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
import Data.Word (Word64)
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

-- | The watchdog of one connection, if it has a timeout. With none
--   there is no watchdog, and recording what the connection does costs
--   nothing, not even a transaction.
newtype H2Watchdog = H2Watchdog (Maybe (Watchdog H2))

-- | Creating one with a timeout in microseconds. Zero or less means the
--   connection never times out.
newH2Watchdog :: Int -> IO H2Watchdog
newH2Watchdog us
    | us <= 0 = return $ H2Watchdog Nothing
    | otherwise =
        H2Watchdog . Just
            <$> W.newWatchdog
                (H2Context (fromIntegral us) $ Activity 0 0 False 0 0)

-- | A write is a flag and the other two are counts, because there is one
--   sender on a connection -- 'Network.HTTP2.H2.Sender.frameSender', run
--   once by 'Network.HTTP2.Server.run' -- and as many applications and
--   waits for the peer as the connection has streams.
data Activity = Activity
    { actRx :: Int
    -- ^ Progress of the peer.
    , actTx :: Int
    -- ^ Completed writes.
    , actSending :: Bool
    -- ^ A write is in progress.
    , actWaiting :: Int
    -- ^ Waits for the peer: request bodies, and windows it has shut.
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
    | actSending a = Writing $ actTx a
    | actWaiting a > 0 = ReadingForApp $ actRx a
    | actApps a > 0 = RunningApp
    | otherwise = Idling $ actRx a

instance WatchdogFor H2 where
    data ContextFor H2 = H2Context
        { h2Timeout :: Word64
        , h2Activity :: Activity
        }

    decide mold new = return $ case mold of
        -- The watchdog starting: the connection has yet to do anything,
        -- and what it is not doing is already on the clock.
        Nothing -> action
        Just old
            | rule (h2Activity old) == r' -> ignore
            | otherwise -> action
      where
        r' = rule $ h2Activity new
        action
            | RunningApp <- r' = unlimited
            | otherwise = setTimeout $ h2Timeout new

-- | Succeeding once the connection has timed out.
timedOutSTM :: H2Watchdog -> STM ()
timedOutSTM (H2Watchdog Nothing) = retry
timedOutSTM (H2Watchdog (Just wd)) = W.timedOutSTM wd

-- | Whether the connection has timed out.
isTimedOut :: H2Watchdog -> IO Bool
isTimedOut (H2Watchdog Nothing) = return False
isTimedOut (H2Watchdog (Just wd)) = W.isTimedOut wd

-- | Supervising a body with the watchdog thread.
withWatchdog :: H2Watchdog -> IO () -> IO a -> IO a
withWatchdog (H2Watchdog Nothing) _ body = body
withWatchdog (H2Watchdog (Just wd)) lastResort body =
    W.withWatchdog wd lastResort body

modifyActivity :: H2Watchdog -> (Activity -> Activity) -> IO ()
modifyActivity (H2Watchdog Nothing) _ = return ()
modifyActivity (H2Watchdog (Just wd)) f =
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
during _ _ (H2Watchdog Nothing) body = body
during begin end wd body =
    E.bracket_ (modifyActivity wd begin) (modifyActivity wd end) body

-- | Running a write.
sending :: H2Watchdog -> IO a -> IO a
sending wd body = do
    r <- during (\a -> a{actSending = True}) (\a -> a{actSending = False}) wd body
    txTick wd
    return r

-- | Running what only the peer can end: a read on behalf of an
--   application, or a wait for a window it has shut.
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
