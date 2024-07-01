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
    incCounter,
    decCounter,
    waitCounter0,
) where

import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import qualified System.TimeManager as T
import UnliftIO.Concurrent
import UnliftIO.Exception
import qualified UnliftIO.Exception as E
import UnliftIO.STM

import Imports

----------------------------------------------------------------

data Command = Stop (Maybe SomeException) | Add ThreadId | Delete ThreadId

-- | Manager to manage the thread and the timer.
data Manager = Manager (TQueue Command) (TVar Int) T.Manager

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
        go q Set.empty
    return $ Manager q cnt timmgr
  where
    go q tset0 = do
        x <- atomically $ readTQueue q
        case x of
            Stop err -> kill tset0 err
            Add newtid ->
                let tset = add newtid tset0
                 in go q tset
            Delete oldtid ->
                let tset = del oldtid tset0
                 in go q tset

-- | Stopping the manager.
stopAfter :: Manager -> IO a -> (Either SomeException a -> IO b) -> IO b
stopAfter (Manager q _ _) action cleanup = do
    mask $ \unmask -> do
        ma <- try $ unmask action
        atomically $ writeTQueue q $ Stop (either Just (const Nothing) ma)
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
    void $ mask_ $ forkIOWithUnmask $ \unmask -> do
        labelMe label
        addMyId mgr
        -- We catch the exception and do not rethrow it: we don't want the
        -- exception printed to stderr.
        io unmask `catch` \(_e :: SomeException) -> return ()
        deleteMyId mgr

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

add :: ThreadId -> Set ThreadId -> Set ThreadId
add tid set = set'
  where
    set' = Set.insert tid set

del :: ThreadId -> Set ThreadId -> Set ThreadId
del tid set = set'
  where
    set' = Set.delete tid set

kill :: Set ThreadId -> Maybe SomeException -> IO ()
kill set err = traverse_ (\tid -> E.throwTo tid $ KilledByHttp2ThreadManager err) set

-- | Killing the IO action of the second argument on timeout.
timeoutKillThread :: Manager -> (T.Handle -> IO a) -> IO a
timeoutKillThread (Manager _ _ tmgr) action = E.bracket register T.cancel action
  where
    register = T.registerKillThread tmgr (return ())

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
