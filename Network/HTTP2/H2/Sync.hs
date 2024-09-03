{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Sync (prepareSync, syncWithSender) where

import Control.Concurrent
import Control.Concurrent.STM
import Network.HTTP.Semantics.IO

import Network.HTTP2.H2.Context
import Network.HTTP2.H2.Queue
import Network.HTTP2.H2.Types
import Network.HTTP2.H2.Window

-- | Two assumptions about how this function is used:
--
-- 1. A separate thread will be running 'syncWithSender' using the @var@ and
--    @sync@ values constructed here.
-- 2. The 'Output' will be enqueued in the 'outputQ' of some 'Context'
--
-- The returned @sync@ function then has the following usage constraints:
--
-- 1. It may only be called with a 'Just' 'OutputType' if there is no 'Output'
--    already enqueued in the 'outputQ' for the given stream.
-- 2. If the function returns 'False', no other 'Output' may be enqueued for
--    this stream (until one has been dequeued).
prepareSync
    :: Stream
    -> OutputType
    -> Maybe (TBQueue StreamingChunk)
    -> IO ((MVar Sync, Maybe OutputType -> IO Bool), Output)
prepareSync strm otyp mtbq = do
    var <- newEmptyMVar
    let sync = makeSync strm mtbq var
        out = Output strm otyp sync
    return ((var, sync), out)

syncWithSender
    :: Context
    -> Stream
    -> MVar Sync
    -- ^ Precondition: When this is filled with an 'Output' for a particular
    -- stream, the 'outputQ' in the 'Context' /must not/ already contain an
    -- 'Output' for that stream.
    -> (Maybe OutputType -> IO Bool)
    -> IO ()
syncWithSender Context{..} strm var sync = loop
  where
    loop = do
        s <- takeMVar var
        case s of
            Done -> return ()
            Cont wait newotyp -> do
                wait
                -- This is justified by the precondition above
                enqueueOutput outputQ $ Output strm newotyp sync
                loop

-- | Postcondition: This will only write to the 'MVar' if:
--
-- 1. You pass 'Just' an 'OutputType'
-- 2. The return value is 'False'
makeSync
    :: Stream
    -> Maybe (TBQueue StreamingChunk)
    -> MVar Sync
    -> Maybe OutputType
    -> IO Bool
makeSync _ _ var Nothing = putMVar var Done >> return False
makeSync strm mtbq var (Just otyp) = do
    mwait <- checkOpen strm mtbq
    case mwait of
        Nothing -> return True
        Just wait -> do
            putMVar var $ Cont wait otyp
            return False

checkOpen :: Stream -> Maybe (TBQueue StreamingChunk) -> IO (Maybe (IO ()))
checkOpen strm mtbq = case mtbq of
    Nothing -> checkStreamWindowSize
    Just tbq -> checkStreaming tbq
  where
    checkStreaming tbq = do
        isEmpty <- atomically $ isEmptyTBQueue tbq
        if isEmpty
            then do
                return $ Just (waitStreaming tbq)
            else checkStreamWindowSize
    -- FLOW CONTROL: WINDOW_UPDATE: send: respecting peer's limit
    checkStreamWindowSize = do
        sws <- getStreamWindowSize strm
        if sws <= 0
            then return $ Just (waitStreamWindowSize strm)
            else return Nothing

{-# INLINE waitStreaming #-}
waitStreaming :: TBQueue a -> IO ()
waitStreaming tbq = atomically $ do
    isEmpty <- isEmptyTBQueue tbq
    check (not isEmpty)
