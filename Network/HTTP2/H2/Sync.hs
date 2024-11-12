{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Sync (
    LoopCheck (..),
    newLoopCheck,
    syncWithSender,
    syncWithSender',
    makeOutput,
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
syncWithSender ctx@Context{..} strm otyp lc = do
    (var, out) <- makeOutput strm otyp
    enqueueOutput outputQ out
    syncWithSender' ctx var lc

makeOutput :: Stream -> OutputType -> IO (MVar Sync, Output)
makeOutput strm otyp = do
    var <- newEmptyMVar
    let out =
            Output
                { outputStream = strm
                , outputType = otyp
                , outputSync = putMVar var
                }
    return (var, out)

syncWithSender' :: Context -> MVar Sync -> LoopCheck -> IO ()
syncWithSender' Context{..} var lc = loop
  where
    loop = do
        s <- takeMVar var
        case s of
            Done -> return ()
            Cont newout -> do
                cont <- checkLoop lc
                when cont $ do
                    -- This is justified by the precondition above
                    enqueueOutput outputQ newout
                    loop

newLoopCheck :: Stream -> Maybe (TBQueue StreamingChunk) -> IO LoopCheck
newLoopCheck strm mtbq = do
    tovar <- newTVarIO False
    return $
        LoopCheck
            { lcTBQ = mtbq
            , lcTimeout = tovar
            , lcWindow = streamTxFlow strm
            }

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
