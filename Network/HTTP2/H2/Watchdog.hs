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
--   3. Otherwise, while an application is running -- a handler, or the
--      server answering a request of ours -- how long it takes is the
--      application's business, but the peer must still be there: when it
--      has been quiet for half the timeout, a PING asks it.
--
--   4. Otherwise the connection is idle and the peer must make progress.
--
--   Progress is what is reported with 'rxTick' (the peer) and 'sending'
--   (a write). Under every rule, progress restarts the timer, and
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
    setProbe,
    isIdling,
    timedOutSTM,
    isTimedOut,
    withWatchdog,
) where

import Control.Concurrent.STM (
    STM,
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVarIO,
    retry,
 )
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

-- | The watchdog of one connection, if it has a timeout, with what it
--   was last told, to be asked after it timed out. With none there is no
--   watchdog, and recording what the connection does costs nothing, not
--   even a transaction.
newtype H2Watchdog = H2Watchdog (Maybe (Watchdog H2, TVar Activity))

-- | Creating one with a timeout in microseconds. Zero or less means the
--   connection never times out.
newH2Watchdog :: Int -> IO H2Watchdog
newH2Watchdog us
    | us <= 0 = return $ H2Watchdog Nothing
    | otherwise = do
        wd <- W.newWatchdog $ H2Context (fromIntegral us) (return ()) activity0
        mirror <- newTVarIO activity0
        return $ H2Watchdog $ Just (wd, mirror)
  where
    activity0 = Activity 0 0 False 0 0

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
    | RunningApp Int
    | Idling Int
    deriving (Eq)

rule :: Activity -> Rule
rule a
    | actSending a = Writing $ actTx a
    | actWaiting a > 0 = ReadingForApp $ actRx a
    | actApps a > 0 = RunningApp $ actRx a
    | otherwise = Idling $ actRx a

instance WatchdogFor H2 where
    data ContextFor H2 = H2Context
        { h2Timeout :: Word64
        , h2Probe :: IO ()
        , -- \^ Asking the peer for a sign of life: enqueueing a PING.
          h2Activity :: Activity
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
            | RunningApp _ <- r' = setTimeoutProbing (h2Timeout new) (h2Probe new)
            | otherwise = setTimeout $ h2Timeout new

-- | Succeeding once the connection has timed out.
timedOutSTM :: H2Watchdog -> STM ()
timedOutSTM (H2Watchdog Nothing) = retry
timedOutSTM (H2Watchdog (Just (wd, _))) = W.timedOutSTM wd

-- | Whether the connection has timed out.
isTimedOut :: H2Watchdog -> IO Bool
isTimedOut (H2Watchdog Nothing) = return False
isTimedOut (H2Watchdog (Just (wd, _))) = W.isTimedOut wd

-- | Supervising a body with the watchdog thread.
withWatchdog :: H2Watchdog -> IO () -> IO a -> IO a
withWatchdog (H2Watchdog Nothing) _ body = body
withWatchdog (H2Watchdog (Just (wd, _))) lastResort body =
    W.withWatchdog wd lastResort body

modifyActivity :: H2Watchdog -> (Activity -> Activity) -> IO ()
modifyActivity (H2Watchdog Nothing) _ = return ()
modifyActivity (H2Watchdog (Just (wd, mirror))) f = atomically $ do
    modifyTVar' mirror f
    updateSTM (\c -> c{h2Activity = f $ h2Activity c}) wd

-- | Telling the watchdog how to ask the peer for a sign of life, once
--   the connection can send.
setProbe :: H2Watchdog -> IO () -> IO ()
setProbe (H2Watchdog Nothing) _ = return ()
setProbe (H2Watchdog (Just (wd, _))) probe = update (\c -> c{h2Probe = probe}) wd

-- | Whether the connection was idle (rule 4): nothing written, waited
--   for or running.  A timeout then is the connection merely unused.
isIdling :: H2Watchdog -> IO Bool
isIdling (H2Watchdog Nothing) = return False
isIdling (H2Watchdog (Just (_, mirror))) = isIdle . rule <$> readTVarIO mirror
  where
    isIdle (Idling _) = True
    isIdle _ = False

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
