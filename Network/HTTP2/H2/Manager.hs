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
    withTimeout,
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
import qualified System.TimeManager as T

import Imports

----------------------------------------------------------------

-- | Manager to manage the thread and the timer.
data Manager = Manager T.Manager (TVar ManagedThreads)

type ManagedThreads = Map ThreadId TimeoutHandle

----------------------------------------------------------------

data TimeoutHandle
    = ThreadWithTimeout T.Handle
    | ThreadWithoutTimeout

cancelTimeout :: TimeoutHandle -> IO ()
cancelTimeout (ThreadWithTimeout th) = T.cancel th
cancelTimeout ThreadWithoutTimeout = return ()

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
        forM_ (Map.elems m) cancelTimeout
        let er = either Just (const Nothing) ma
        forM_ (Map.keys m) $ \tid ->
            E.throwTo tid $ KilledByHttp2ThreadManager er
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
    void $ mask_ $ forkIOWithUnmask $ \unmask -> E.handle handler $ do
        labelMe label
        tid <- myThreadId
        atomically $ modifyTVar var $ Map.insert tid ThreadWithoutTimeout
        -- We catch the exception and do not rethrow it: we don't want the
        -- exception printed to stderr.
        io unmask `catch` \(_e :: SomeException) -> return ()
        atomically $ modifyTVar var $ Map.delete tid
  where
    handler (E.SomeException _) = return ()

waitCounter0 :: Manager -> IO ()
waitCounter0 (Manager _timmgr var) = atomically $ do
    m <- readTVar var
    check (Map.size m == 0)

----------------------------------------------------------------

withTimeout :: Manager -> (T.Handle -> IO ()) -> IO ()
withTimeout (Manager timmgr var) action =
    E.handle handler $
        E.bracket register unregister action
  where
    register = do
        tid <- myThreadId
        -- throwing TimeoutThread
        th <- T.registerKillThread timmgr $ return ()
        -- overriding ThreadWithoutTimeout
        atomically $ modifyTVar var $ Map.insert tid $ ThreadWithTimeout th
        return th
    unregister th = T.cancel th
    handler T.TimeoutThread = return ()
