-- | A thread pool manager.
--   The manager has responsibility to spawn and kill
--   worker threads.
module Network.HTTP2.Arch.Manager (
    Manager
  , Action
  , start
  , setAction
  , stop
  , spawnAction
  , forkManaged
  , deleteMyId
  , timeoutKillThread
  , timeoutClose
  ) where

import Control.Exception
import Data.Foldable
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set
import qualified System.TimeManager as T
import UnliftIO.Concurrent
import qualified UnliftIO.Exception as E
import UnliftIO.STM

import Imports

----------------------------------------------------------------

-- | Action to be spawned by the manager.
type Action = IO ()

noAction :: Action
noAction = return ()

data Command = Stop | Spawn | Add ThreadId | Delete ThreadId

-- | Manager to manage the thread pool and the timer.
data Manager = Manager (TQueue Command) (IORef Action) T.Manager

-- | Starting a thread pool manager.
--   Its action is initially set to 'return ()' and should be set
--   by 'setAction'. This allows that the action can include
--   the manager itself.
start :: T.Manager -> IO Manager
start timmgr = do
    q <- newTQueueIO
    ref <- newIORef noAction
    void $ forkIO $ go q Set.empty ref
    return $ Manager q ref timmgr
  where
    go q tset0 ref = do
        x <- atomically $ readTQueue q
        case x of
            Stop          -> kill tset0
            Spawn         -> next tset0
            Add    newtid -> let tset = add newtid tset0
                             in go q tset ref
            Delete oldtid -> let tset = del oldtid tset0
                             in go q tset ref
      where
        next tset = do
            action <- readIORef ref
            newtid <- forkIO action
            let tset' = add newtid tset
            go q tset' ref

-- | Setting the action to be spawned.
setAction :: Manager -> Action -> IO ()
setAction (Manager _ ref _) action = writeIORef ref action

-- | Stopping the manager.
stop :: Manager -> IO ()
stop (Manager q _ _) = atomically $ writeTQueue q Stop

-- | Spawning the action.
spawnAction :: Manager -> IO ()
spawnAction (Manager q _ _) = atomically $ writeTQueue q Spawn

----------------------------------------------------------------

-- | Fork managed thread
--
-- This guarantees that the thread ID is added to the manager's queue before
-- the thread starts, and is removed again when the thread terminates
-- (normally or abnormally).
forkManaged :: Manager -> IO () -> IO ()
forkManaged mgr io =
    void $ mask_ $ forkIOWithUnmask $ \unmask -> do
      addMyId mgr
      r <- unmask io `onException` deleteMyId mgr
      deleteMyId mgr
      return r

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

kill :: Set ThreadId -> IO ()
kill set = traverse_ killThread set

-- | Killing the IO action of the second argument on timeout.
timeoutKillThread :: Manager -> (T.Handle -> IO ()) -> IO ()
timeoutKillThread (Manager _ _ tmgr) action = E.bracket register T.cancel action
  where
    register = T.registerKillThread tmgr noAction

-- | Registering closer for a resource and
--   returning a timer refresher.
timeoutClose :: Manager -> IO () -> IO (IO ())
timeoutClose (Manager _ _ tmgr) closer = do
    th <- T.register tmgr closer
    return $ T.tickle th
