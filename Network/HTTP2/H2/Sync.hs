{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Sync (prepareSync, syncWithSender) where

import Control.Concurrent
import Network.HTTP.Semantics.IO
import UnliftIO.STM

import Network.HTTP2.H2.Context
import Network.HTTP2.H2.Queue
import Network.HTTP2.H2.Types
import Network.HTTP2.H2.Window

prepareSync
    :: Stream
    -> OutputType
    -> Maybe (TBQueue StreamingChunk)
    -> IO ((MVar Sync, Maybe OutputType -> IO Bool), Output)
prepareSync strm otyp mtbq = do
    var <- newEmptyMVar
    let sync = makeSync strm mtbq (putMVar var)
        out = Output strm otyp sync
    return ((var, sync), out)

syncWithSender
    :: Context
    -> Stream
    -> (MVar Sync, Maybe OutputType -> IO Bool)
    -> IO ()
syncWithSender Context{..} strm (var, sync) = loop
  where
    loop = do
        s <- takeMVar var
        case s of
            Done -> return ()
            Cont wait newotyp -> do
                wait
                enqueueOutput outputQ $ Output strm newotyp sync
                loop

makeSync
    :: Stream
    -> Maybe (TBQueue StreamingChunk)
    -> (Sync -> IO ())
    -> Maybe OutputType
    -> IO Bool
makeSync _ _ sync Nothing = sync Done >> return False
makeSync strm mtbq sync (Just otyp) = do
    mwait <- checkOpen strm mtbq
    case mwait of
        Nothing -> return True
        Just wait -> do
            sync $ Cont wait otyp
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
    checkSTM (not isEmpty)
