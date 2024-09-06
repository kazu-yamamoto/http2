{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Sync (
    LoopCheck (..),
    syncWithSender,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Network.Control
import Network.HTTP.Semantics.IO

import Network.HTTP2.H2.Context
import Network.HTTP2.H2.Queue
import Network.HTTP2.H2.Types

syncWithSender
    :: Context
    -> Stream
    -> OutputType
    -> LoopCheck
    -> IO ()
syncWithSender Context{..} strm otyp lc = do
    var <- newEmptyMVar
    let out = Output strm otyp var
    enqueueOutput outputQ out
    loop var
  where
    loop var = do
        s <- takeMVar var
        case s of
            Done -> return ()
            Cont newout -> do
                cont <- checkLoop lc
                when cont $ do
                    -- This is justified by the precondition above
                    enqueueOutput outputQ newout
                    loop var

data LoopCheck = LoopCheck
    { lcTBQ :: Maybe (TBQueue StreamingChunk)
    , lcTimeout :: TVar Bool
    , lcWindow :: TVar TxFlow
    }

checkLoop :: LoopCheck -> IO Bool
checkLoop LoopCheck{..} = atomically $ do
    tout <- readTVar lcTimeout
    if tout
        then return False
        else do
            waitStreaming' lcTBQ
            waitStreamWindowSizeSTM lcWindow
            return True

waitStreaming' :: Maybe (TBQueue a) -> STM ()
waitStreaming' Nothing = return ()
waitStreaming' (Just tbq) = do
    isEmpty <- isEmptyTBQueue tbq
    check (not isEmpty)

waitStreamWindowSizeSTM :: TVar TxFlow -> STM ()
waitStreamWindowSizeSTM txf = do
    w <- txWindowSize <$> readTVar txf
    check (w > 0)
