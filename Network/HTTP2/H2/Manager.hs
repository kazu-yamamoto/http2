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
    timeoutClose,
    KilledByHttp2ThreadManager (..),
    waitCounter0,
) where

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified System.TimeManager as T
import UnliftIO.Concurrent
import UnliftIO.Exception
import qualified UnliftIO.Exception as E
import UnliftIO.STM

import Imports

----------------------------------------------------------------

data Command =
    Stop (MVar ()) (Maybe SomeException)
  | Add ThreadId
  | RegisterTimeout ThreadId T.Handle
  | Delete ThreadId

-- | Manager to manage the thread and the timer.
data Manager = Manager (TQueue Command) (TVar Int) T.Manager

data TimeoutHandle =
    ThreadWithTimeout T.Handle
  | ThreadWithoutTimeout

cancelTimeout :: TimeoutHandle -> IO ()
cancelTimeout (ThreadWithTimeout h) = T.cancel h
cancelTimeout ThreadWithoutTimeout  = return ()

type ManagedThreads = Map ThreadId TimeoutHandle

-- | Starting a thread manager.
--   Its action is initially set to 'return ()' and should be set
--   by 'setAction'. This allows that the action can include
--   the manager itself.
start :: T.Manager -> IO Manager
start timmgr = do
    q <- newTQueueIO
    cnt <- newTVarIO 0
    void $ forkIO $ do
        labelMe "H2 thread manager"
        go q Map.empty
    return $ Manager q cnt timmgr
  where
    -- This runs in a separate thread whose ThreadId is not known by anyone
    -- else, so it cannot be killed by asynchronous exceptions.
    go :: TQueue Command -> ManagedThreads -> IO ()
    go q threadMap0 = do
        x <- atomically $ readTQueue q
        case x of
            Stop signalDone err -> do
                kill threadMap0 err
                putMVar signalDone ()
            Add newtid -> do
                let threadMap = add newtid threadMap0
                go q threadMap
            RegisterTimeout tid h -> do
                let threadMap = registerTimeout tid h threadMap0
                go q threadMap
            Delete oldtid -> do
                threadMap <- del oldtid threadMap0
                go q threadMap

-- | Stopping the manager.
stopAfter :: Manager -> IO a -> (Either SomeException a -> IO b) -> IO b
stopAfter (Manager q _ _) action cleanup = do
    mask $ \unmask -> do
        ma <- try $ unmask action
        signalDone <- newEmptyMVar
        atomically $ writeTQueue q $ Stop signalDone (either Just (const Nothing) ma)
        -- This call to takeMVar /will/ eventually succeed, because the Manager
        -- thread cannot be killed (see comment on 'go' in 'start').
        takeMVar signalDone
        cleanup ma

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
    void $ mask_ $ forkIOWithUnmask $ \unmask -> E.handleSyncOrAsync handler $ do
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
addMyId (Manager q _ _) = do
    tid <- myThreadId
    atomically $ writeTQueue q $ Add tid

-- | Deleting my thread id from the kill-thread list on stopping.
--
-- This is /only/ necessary when you want to remove the thread's ID from
-- the manager /before/ the thread terminates (thereby assuming responsibility
-- for thread cleanup yourself).
deleteMyId :: Manager -> IO ()
deleteMyId (Manager q _ _) = do
    tid <- myThreadId
    atomically $ writeTQueue q $ Delete tid

----------------------------------------------------------------

add :: ThreadId -> ManagedThreads -> ManagedThreads
add tid = Map.insert tid ThreadWithoutTimeout

registerTimeout :: ThreadId -> T.Handle -> ManagedThreads -> ManagedThreads
registerTimeout tid = Map.insert tid . ThreadWithTimeout

del :: ThreadId -> ManagedThreads -> IO ManagedThreads
del tid threadMap = do
    forM_ (Map.lookup tid threadMap) cancelTimeout
    return $ Map.delete tid threadMap

kill :: ManagedThreads -> Maybe SomeException -> IO ()
kill threadMap err =
    forM_ (Map.toList threadMap) $ \(tid, mHandle) -> do
      cancelTimeout mHandle
      E.throwTo tid $ KilledByHttp2ThreadManager err

-- | Killing the IO action of the second argument on timeout.
timeoutKillThread :: Manager -> (T.Handle -> IO a) -> IO a
timeoutKillThread (Manager q _ tmgr) action = E.bracket register T.cancel action
  where
    register = do
      h <- T.registerKillThread tmgr (return ())
      tid <- myThreadId
      atomically $ writeTQueue q (RegisterTimeout tid h)
      return h

-- | Registering closer for a resource and
--   returning a timer refresher.
timeoutClose :: Manager -> IO () -> IO (IO ())
timeoutClose (Manager _ _ tmgr) closer = do
    th <- T.register tmgr closer
    return $ T.tickle th

data KilledByHttp2ThreadManager = KilledByHttp2ThreadManager (Maybe SomeException)
    deriving (Show)

instance Exception KilledByHttp2ThreadManager where
    toException = asyncExceptionToException
    fromException = asyncExceptionFromException

----------------------------------------------------------------

incCounter :: Manager -> IO ()
incCounter (Manager _ cnt _) = atomically $ modifyTVar' cnt (+ 1)

decCounter :: Manager -> IO ()
decCounter (Manager _ cnt _) = atomically $ modifyTVar' cnt (subtract 1)

waitCounter0 :: Manager -> IO ()
waitCounter0 (Manager _ cnt _) = atomically $ do
    n <- readTVar cnt
    checkSTM (n < 1)
