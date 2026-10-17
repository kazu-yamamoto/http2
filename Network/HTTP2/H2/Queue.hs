module Network.HTTP2.H2.Queue where

import Control.Concurrent.STM

import Network.HTTP2.H2.Types

{-# INLINE enqueueOutput #-}
enqueueOutput :: TQueue Output -> Output -> IO ()
enqueueOutput outQ out = atomically $ writeTQueue outQ out

{-# INLINE enqueueOutputSTM #-}
enqueueOutputSTM :: TQueue Output -> Output -> STM ()
enqueueOutputSTM outQ out = writeTQueue outQ out

newControlQ :: IO ControlQ
newControlQ = ControlQ <$> newTQueueIO <*> newTVarIO 0

-- | Never blocks: the sender itself enqueues control frames (resets), and
-- would wait for ever on a queue only it empties.
{-# INLINE enqueueControl #-}
enqueueControl :: ControlQ -> Control -> IO ()
enqueueControl (ControlQ q n) ctl = atomically $ do
    writeTQueue q ctl
    modifyTVar' n (+ 1)

dequeueControlSTM :: ControlQ -> STM Control
dequeueControlSTM (ControlQ q n) = do
    ctl <- readTQueue q
    modifyTVar' n (subtract 1)
    return ctl

isEmptyControlQ :: ControlQ -> STM Bool
isEmptyControlQ (ControlQ q _) = isEmptyTQueue q

-- | Wait until fewer than 'controlQLimit' control frames are waiting for the
-- sender.  The receiver does, before reading each frame.
--
-- Most of the control frames are answers to what the peer sends:
-- RST_STREAM(REFUSED_STREAM) to a stream over our
-- SETTINGS_MAX_CONCURRENT_STREAMS, PING and SETTINGS acks, WINDOW_UPDATE.
-- A peer that does not read them blocks the sender on the socket, and if
-- we read on regardless, they pile up here without bound: a refused stream
-- is a few octets of HEADERS, and not rate-limited, since a peer is meant
-- to be able to retry it.  So we stop reading a peer that does not read
-- us, until the sender has caught up -- or the receive timeout gives up on
-- it.  A peer that reads loses nothing: it could not go on sending without
-- our WINDOW_UPDATEs anyway.
waitControlQ :: ControlQ -> STM ()
waitControlQ (ControlQ _ n) = do
    len <- readTVar n
    check (len < controlQLimit)

-- | How many control frames may wait for the sender before the receiver
-- stops reading: some 150 KiB of them.
controlQLimit :: Int
controlQLimit = 1000

----------------------------------------------------------------
