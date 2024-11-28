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
    forkManagedTimeout,
    KilledByHttp2ThreadManager (..),
    waitCounter0,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import qualified Control.Exception as E
import Data.Foldable
import Data.IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as Map
import System.Mem.Weak (Weak, deRefWeak)
import qualified System.TimeManager as T

import Imports

----------------------------------------------------------------

-- | Manager to manage the thread and the timer.
data Manager = Manager T.Manager (TVar ManagedThreads)

type ManagedThreads = IntMap ManagedThread

----------------------------------------------------------------

-- 'IORef' prevents race between WAI TimeManager (TimeoutThread)
-- and stopAfter (KilledByHttp2ThreadManager).
-- It is initialized with 'False' and turned into 'True' when locked.
-- The winner can throw an asynchronous exception.
data ManagedThread = ManagedThread (Weak ThreadId) (IORef Bool)

----------------------------------------------------------------

-- | Starting a thread manager.
--   Its action is initially set to 'return ()' and should be set
--   by 'setAction'. This allows that the action can include
--   the manager itself.
start :: T.Manager -> IO Manager
start timmgr = Manager timmgr <$> newTVarIO Map.empty

----------------------------------------------------------------

data KilledByHttp2ThreadManager = KilledByHttp2ThreadManager (Maybe SomeException)
    deriving (Show)

instance Exception KilledByHttp2ThreadManager where
    toException = asyncExceptionToException
    fromException = asyncExceptionFromException

-- | Stopping the manager.
--
-- The action is run in the scope of an exception handler that catches all
-- exceptions (including asynchronous ones); this allows the cleanup handler
-- to cleanup in all circumstances. If an exception is caught, it is rethrown
-- after the cleanup is complete.
stopAfter :: Manager -> IO a -> (Maybe SomeException -> IO ()) -> IO a
stopAfter (Manager _timmgr var) action cleanup = do
    mask $ \unmask -> do
        ma <- try $ unmask action
        m <- atomically $ do
            m0 <- readTVar var
            writeTVar var Map.empty
            return m0
        let ths = Map.elems m
            er = either Just (const Nothing) ma
            ex = KilledByHttp2ThreadManager er
        forM_ ths $ \(ManagedThread wtid ref) -> lockAndKill wtid ref ex
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
forkManagedUnmask (Manager _timmgr var) label io =
    void $ mask_ $ forkIOWithUnmask $ \unmask -> E.handle ignore $ do
        labelMe label
        E.bracket (setup var) (clear var) $ \_ -> io unmask

forkManagedTimeout :: Manager -> String -> (T.Handle -> IO ()) -> IO ()
forkManagedTimeout (Manager timmgr var) label io =
    void $ forkIO $ E.handle ignore $ do
        labelMe label
        E.bracket (setup var) (clear var) $ \(_n, wtid, ref) ->
            -- 'TimeoutThread' is ignored by 'withHandle'.
            T.withHandle timmgr (lockAndKill wtid ref T.TimeoutThread) io

setup :: TVar (IntMap ManagedThread) -> IO (Int, Weak ThreadId, IORef Bool)
setup var = do
    (wtid, n) <- myWeakThradId
    ref <- newIORef False
    let ent = ManagedThread wtid ref
    -- asking to throw KilledByHttp2ThreadManager to me
    atomically $ modifyTVar' var $ Map.insert n ent
    return (n, wtid, ref)

lockAndKill :: Exception e => Weak ThreadId -> IORef Bool -> e -> IO ()
lockAndKill wtid ref e = do
    alreadyLocked <- atomicModifyIORef' ref (\b -> (True, b)) -- try to lock
    unless alreadyLocked $ do
        mtid <- deRefWeak wtid
        case mtid of
            Nothing -> return ()
            Just tid -> E.throwTo tid e

clear
    :: TVar (IntMap ManagedThread)
    -> (Map.Key, Weak ThreadId, IORef Bool)
    -> IO ()
clear var (n, _, _) = atomically $ modifyTVar' var $ Map.delete n

ignore :: KilledByHttp2ThreadManager -> IO ()
ignore (KilledByHttp2ThreadManager _) = return ()

waitCounter0 :: Manager -> IO ()
waitCounter0 (Manager _timmgr var) = atomically $ do
    m <- readTVar var
    check (Map.size m == 0)

----------------------------------------------------------------

myWeakThradId :: IO (Weak ThreadId, Int)
myWeakThradId = do
    tid <- myThreadId
    wtid <- mkWeakThreadId tid
    let n = read (drop 9 $ show tid) -- drop "ThreadId "
    return (wtid, n)
