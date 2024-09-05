{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A thread manager.
--   The manager has responsibility to kill worker threads.
module Network.HTTP2.H2.Manager (
    Manager,
    start,
    stopAfter,
    forkManaged,
    forkManagedUnmask,
    timeoutKillThread,
    KilledByHttp2ThreadManager (..),
    waitCounter0,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import qualified Control.Exception as E
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import GHC.Event

import Imports

----------------------------------------------------------------

data Command
    = Stop (MVar ()) (Maybe SomeException)
    | Add ThreadId
    | RegisterTimeout ThreadId TimeoutKey
    | Delete ThreadId

-- | Manager to manage the thread and the timer.
data Manager = Manager (TQueue Command) (TVar Int)

data TimeoutHandle
    = ThreadWithTimeout TimeoutKey
    | ThreadWithoutTimeout

cancelTimeout :: TimeoutHandle -> IO ()
cancelTimeout (ThreadWithTimeout key) = do
    timmgr <- getSystemTimerManager
    unregisterTimeout timmgr key
cancelTimeout ThreadWithoutTimeout = return ()

type ManagedThreads = Map ThreadId TimeoutHandle

-- | Starting a thread manager.
--   Its action is initially set to 'return ()' and should be set
--   by 'setAction'. This allows that the action can include
--   the manager itself.
start :: IO Manager
start = do
    q <- newTQueueIO
    cnt <- newTVarIO 0
    void $ forkIO $ do
        labelMe "H2 thread manager"
        go q Map.empty
    return $ Manager q cnt
  where
    -- This runs in a separate thread whose ThreadId is not known by anyone
    -- else, so it cannot be killed by asynchronous exceptions.
    go :: TQueue Command -> ManagedThreads -> IO ()
    go q threadMap0 = do
        x <- atomically $ readTQueue q
        case x of
            Stop signalTimeoutsDisabled err -> do
                -- We first remove all threads from the timeout
                -- manager, then signal that that is complete, and
                -- finally kill all threads. This avoids a race
                -- between the timeout manager and our manager: we
                -- want to ensure that the exception that gets
                -- delivered is 'KilledByHttp2ThreadManager', not
                -- 'TimeoutThread'.
                forM_ (Map.elems threadMap0) cancelTimeout
                putMVar signalTimeoutsDisabled ()
                forM_ (Map.keys threadMap0) $ \tid ->
                    E.throwTo tid $ KilledByHttp2ThreadManager err
            Add newtid -> do
                let threadMap = Map.insert newtid ThreadWithoutTimeout threadMap0
                go q threadMap
            Delete oldtid -> do
                forM_ (Map.lookup oldtid threadMap0) cancelTimeout
                let threadMap = Map.delete oldtid threadMap0
                go q threadMap
            RegisterTimeout tid h -> do
                let threadMap = Map.insert tid (ThreadWithTimeout h) threadMap0
                go q threadMap

-- | Stopping the manager.
--
-- The action is run in the scope of an exception handler that catches all
-- exceptions (including asynchronous ones); this allows the cleanup handler
-- to cleanup in all circumstances. If an exception is caught, it is rethrown
-- after the cleanup is complete.
stopAfter :: Manager -> IO a -> (Maybe SomeException -> IO ()) -> IO a
stopAfter (Manager q _) action cleanup = do
    mask $ \unmask -> do
        ma <- try $ unmask action
        signalTimeoutsDisabled <- newEmptyMVar
        atomically $
            writeTQueue q $
                Stop signalTimeoutsDisabled (either Just (const Nothing) ma)
        -- This call to takeMVar /will/ eventually succeed, because the Manager
        -- thread cannot be killed (see comment on 'go' in 'start').
        takeMVar signalTimeoutsDisabled
        case ma of
            Left err -> cleanup (Just err) >> throwIO err
            Right a -> cleanup Nothing >> return a

----------------------------------------------------------------

-- | Fork managed thread
--
-- This guarantees that the thread ID is added to the manager's queue before
-- the thread starts, and is removed again when the thread terminates
-- (normally or abnormally).
forkManaged :: Manager -> String -> IO () -> IO ()
forkManaged mgr label io =
    forkManagedUnmask mgr label $ \unmask -> unmask io

-- | Like 'forkManaged', but run action with exceptions masked
forkManagedUnmask
    :: Manager -> String -> ((forall x. IO x -> IO x) -> IO ()) -> IO ()
forkManagedUnmask mgr label io =
    void $ mask_ $ forkIOWithUnmask $ \unmask -> E.handle handler $ do
        labelMe label
        addMyId mgr
        incCounter mgr
        -- We catch the exception and do not rethrow it: we don't want the
        -- exception printed to stderr.
        io unmask `catch` \(_e :: SomeException) -> return ()
        deleteMyId mgr
        decCounter mgr
  where
    handler (E.SomeException _) = return ()

-- | Adding my thread id to the kill-thread list on stopping.
--
-- This is not part of the public API; see 'forkManaged' instead.
addMyId :: Manager -> IO ()
addMyId (Manager q _) = do
    tid <- myThreadId
    atomically $ writeTQueue q $ Add tid

-- | Deleting my thread id from the kill-thread list on stopping.
--
-- This is /only/ necessary when you want to remove the thread's ID from
-- the manager /before/ the thread terminates (thereby assuming responsibility
-- for thread cleanup yourself).
deleteMyId :: Manager -> IO ()
deleteMyId (Manager q _) = do
    tid <- myThreadId
    atomically $ writeTQueue q $ Delete tid

----------------------------------------------------------------

-- | Killing the IO action of the second argument on timeout.
timeoutKillThread :: Manager -> (TimeoutKey -> IO a) -> IO a
timeoutKillThread (Manager q _) action =
    E.bracket register unregister action
  where
    register = do
        tid <- myThreadId
        timmgr <- getSystemTimerManager
        key <- registerTimeout timmgr 30000000 $ void $ forkIO $ killThread tid
        atomically $ writeTQueue q (RegisterTimeout tid key)
        return key
    unregister key = do
        timmgr <- getSystemTimerManager
        unregisterTimeout timmgr key

data KilledByHttp2ThreadManager = KilledByHttp2ThreadManager (Maybe SomeException)
    deriving (Show)

instance Exception KilledByHttp2ThreadManager where
    toException = asyncExceptionToException
    fromException = asyncExceptionFromException

----------------------------------------------------------------

incCounter :: Manager -> IO ()
incCounter (Manager _ cnt) = atomically $ modifyTVar' cnt (+ 1)

decCounter :: Manager -> IO ()
decCounter (Manager _ cnt) = atomically $ modifyTVar' cnt (subtract 1)

waitCounter0 :: Manager -> IO ()
waitCounter0 (Manager _ cnt) = atomically $ do
    n <- readTVar cnt
    check (n < 1)
