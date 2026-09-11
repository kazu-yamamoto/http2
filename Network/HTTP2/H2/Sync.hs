{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Sync (
    LoopCheck (..),
    newLoopCheck,
    syncWithSender,
    syncWithSender',
    makeOutput,
    makeOutputIO,
    enqueueOutputSIO,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Network.Control
import Network.HTTP.Semantics.IO
import qualified System.TimeManager as T

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
    (pop, out) <- makeOutput strm otyp
    enqueueOutput outputQ out
    syncWithSender' ctx pop lc

makeOutput :: Stream -> OutputType -> IO (IO Sync, Output)
makeOutput strm otyp = do
    var <- newEmptyMVar
    let push mout = case mout of
            Nothing -> putMVar var Done
            Just ot -> putMVar var $ Cont ot
        pop = takeMVar var
        out =
            Output
                { outputStream = strm
                , outputType = otyp
                , outputSync = push
                }
    return (pop, out)

makeOutputIO :: Context -> Stream -> OutputType -> Output
makeOutputIO Context{..} strm otyp = out
  where
    push mout = case mout of
        Nothing -> return ()
        -- Sender enqueues output again ignoring
        -- the stream TX window.
        Just ot -> enqueueOutput outputQ ot
    out =
        Output
            { outputStream = strm
            , outputType = otyp
            , outputSync = push
            }

enqueueOutputSIO :: Context -> Stream -> OutputType -> IO ()
enqueueOutputSIO ctx@Context{..} strm otyp = do
    let out = makeOutputIO ctx strm otyp
    enqueueOutput outputQ out

syncWithSender' :: Context -> IO Sync -> LoopCheck -> IO ()
syncWithSender' Context{..} pop lc = loop
  where
    loop = do
        s <- pop
        case s of
            Done -> return ()
            Cont newout -> do
                mapM_ T.tickle (lcTimeHandle lc)
                cont <- checkLoop lc
                when cont $ do
                    enqueueOutput outputQ newout
                    loop

newLoopCheck
    :: Stream -> Maybe (TBQueue StreamingChunk) -> Maybe T.Handle -> IO LoopCheck
newLoopCheck strm mtbq mth = do
    tovar <- newTVarIO False
    return $
        LoopCheck
            { lcState = streamState strm
            , lcTBQ = mtbq
            , lcTimeout = tovar
            , lcWindow = streamTxFlow strm
            , lcTimeHandle = mth
            }

data LoopCheck = LoopCheck
    { lcState :: TVar StreamState
    , lcTBQ :: Maybe (TBQueue StreamingChunk)
    , lcTimeout :: TVar Bool
    , lcWindow :: TVar TxFlow
    , lcTimeHandle :: Maybe T.Handle
    }

checkLoop :: LoopCheck -> IO Bool
checkLoop LoopCheck{..} = atomically $ do
    tout <- readTVar lcTimeout
    state <- readTVar lcState
    if
        | tout -> return False
        | Closed{} <- state -> return False
        | otherwise -> do
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
