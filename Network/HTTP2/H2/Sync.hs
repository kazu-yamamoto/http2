{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import qualified System.ThreadManager as T

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

-- | An output for the 'runIO' interfaces, which have no thread waiting to
-- put the rest of a body back on the queue.
--
-- The rest used to go back at once, whatever the stream's window.  With
-- none left, the sender filled a DATA frame into no room; a file read into
-- no room reads 0 octets, which is the end of the file, so a body larger than
-- the window went out cut short with END_STREAM.  A streaming body with
-- nothing queued made the sender spin instead.  So the rest goes back once
-- it can go on, the way 'syncWithSender'' does it for the other interfaces;
-- only when it has to wait is a thread used for it.
makeOutputIO
    :: Context -> Stream -> Maybe (TBQueue StreamingChunk) -> OutputType -> Output
makeOutputIO Context{..} strm mtbq otyp = out
  where
    push mout = case mout of
        Nothing -> return ()
        Just ot -> do
            now <- atomically $ (Just <$> ready) `orElse` return Nothing
            case now of
                Just True -> enqueueOutput outputQ ot
                Just False -> return ()
                Nothing ->
                    T.forkManaged threadManager "H2 output waiting for its window" $ do
                        ok <- atomically ready
                        when ok $ enqueueOutput outputQ ot
    ready = readyToContinue strm mtbq
    out =
        Output
            { outputStream = strm
            , outputType = otyp
            , outputSync = push
            }

-- | Whether the rest of a stream's body can go on: waiting while the
-- stream's window is shut or a streaming body has nothing queued, and 'False'
-- once the stream is closed.
readyToContinue :: Stream -> Maybe (TBQueue StreamingChunk) -> STM Bool
readyToContinue Stream{streamState, streamTxFlow} mtbq = do
    state <- readTVar streamState
    case state of
        Closed{} -> return False
        _ -> do
            waitStreaming' mtbq
            waitStreamWindowSizeSTM streamTxFlow
            return True

enqueueOutputSIO :: Context -> Stream -> OutputType -> IO ()
enqueueOutputSIO ctx@Context{..} strm otyp = do
    let out = makeOutputIO ctx strm Nothing otyp
    enqueueOutput outputQ out

syncWithSender' :: Context -> IO Sync -> LoopCheck -> IO ()
syncWithSender' Context{..} pop lc = loop
  where
    loop = do
        s <- pop
        case s of
            Done -> return ()
            Cont newout -> do
                cont <- checkLoop lc
                when cont $ do
                    enqueueOutput outputQ newout
                    loop

newLoopCheck :: Stream -> Maybe (TBQueue StreamingChunk) -> IO LoopCheck
newLoopCheck strm mtbq = do
    tovar <- newTVarIO False
    return $
        LoopCheck
            { lcState = streamState strm
            , lcTBQ = mtbq
            , lcTimeout = tovar
            , lcWindow = streamTxFlow strm
            }

data LoopCheck = LoopCheck
    { lcState :: TVar StreamState
    , lcTBQ :: Maybe (TBQueue StreamingChunk)
    , lcTimeout :: TVar Bool
    , lcWindow :: TVar TxFlow
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
