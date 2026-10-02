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
import Network.HTTP2.H2.Watchdog (H2Watchdog, waitingForPeer)

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
            now <- atomically $ (Just <$> ready (return Nothing)) `orElse` return Nothing
            case now of
                Just (Just True) -> enqueueOutput outputQ ot
                Just (Just False) -> return ()
                _ ->
                    T.forkManaged threadManager "H2 output waiting for its window" $ do
                        ok <- waitToContinue watchdog ready
                        when ok $ enqueueOutput outputQ ot
    ready = readyToContinue strm mtbq
    out =
        Output
            { outputStream = strm
            , outputType = otyp
            , outputSync = push
            }

-- | Whether the rest of a stream's body can go on: waiting while a
-- streaming body has nothing queued, and 'False' once the stream is closed.
-- While the stream's window is shut, it is the third argument.
readyToContinue
    :: Stream
    -> Maybe (TBQueue StreamingChunk)
    -> STM (Maybe Bool)
    -> STM (Maybe Bool)
readyToContinue Stream{streamState, streamTxFlow} mtbq shut = do
    state <- readTVar streamState
    case state of
        Closed{} -> return $ Just False
        _ -> do
            waitStreaming' mtbq
            open <- isStreamWindowOpen streamTxFlow
            if open then return (Just True) else shut

-- | Waiting for one of the above to decide.  Only the peer can open a shut
-- window, so that wait is the peer's to end, unlike one for the application.
waitToContinue
    :: H2Watchdog -> (STM (Maybe Bool) -> STM (Maybe Bool)) -> IO Bool
waitToContinue wd ready = do
    now <- atomically $ ready (return Nothing)
    case now of
        Just ok -> return ok
        Nothing -> waitingForPeer wd $ atomically $ ready retry >>= maybe retry return

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
                cont <- checkLoop watchdog lc
                when cont $ do
                    enqueueOutput outputQ newout
                    loop

newLoopCheck :: Stream -> Maybe (TBQueue StreamingChunk) -> IO LoopCheck
newLoopCheck strm mtbq =
    return $
        LoopCheck
            { lcState = streamState strm
            , lcTBQ = mtbq
            , lcWindow = streamTxFlow strm
            }

data LoopCheck = LoopCheck
    { lcState :: TVar StreamState
    , lcTBQ :: Maybe (TBQueue StreamingChunk)
    , lcWindow :: TVar TxFlow
    }

checkLoop :: H2Watchdog -> LoopCheck -> IO Bool
checkLoop wd LoopCheck{..} = waitToContinue wd $ \shut -> do
    state <- readTVar lcState
    case state of
        Closed{} -> return $ Just False
        _ -> do
            waitStreaming' lcTBQ
            open <- isStreamWindowOpen lcWindow
            if open then return (Just True) else shut

waitStreaming' :: Maybe (TBQueue a) -> STM ()
waitStreaming' Nothing = return ()
waitStreaming' (Just tbq) = do
    isEmpty <- isEmptyTBQueue tbq
    check (not isEmpty)

isStreamWindowOpen :: TVar TxFlow -> STM Bool
isStreamWindowOpen txf = (> 0) . txWindowSize <$> readTVar txf
