{-# LANGUAGE BangPatterns #-}

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
  , addMyId
  , deleteMyId
  , timeoutKillThread
  , timeoutClose
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Data.Foldable
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set
import qualified System.TimeManager as T

import Imports

----------------------------------------------------------------

type Action = IO ()

noAction :: Action
noAction = return ()

data Command = Stop | Spawn | Add ThreadId | Delete ThreadId

data Manager = Manager (TQueue Command) (IORef Action) T.Manager

-- | Starting a thread pool manager.
--   Its action is initially set to 'return ()' and should be set
--   by 'setAction'. This allows that the action can include
--   the manager itself.
start :: IO Manager
start = do
    q <- newTQueueIO
    ref <- newIORef noAction
    timmgr <- T.initialize $ 30 * 1000000 -- fixme
    void $ forkIO $ go q Set.empty ref timmgr
    return $ Manager q ref timmgr
  where
    go q !tset0 ref timmgr = do
        x <- atomically $ readTQueue q
        case x of
            Stop          -> kill tset0 >> T.killManager timmgr
            Spawn         -> next tset0
            Add    newtid -> let !tset = add newtid tset0
                             in go q tset ref timmgr
            Delete oldtid -> let !tset = del oldtid tset0
                             in go q tset ref timmgr
      where
        next tset = do
            action <- readIORef ref
            newtid <- forkIO action
            let !tset' = add newtid tset
            go q tset' ref timmgr

setAction :: Manager -> Action -> IO ()
setAction (Manager _ ref _) action = writeIORef ref action

stop :: Manager -> IO ()
stop (Manager q _ _) = atomically $ writeTQueue q Stop

spawnAction :: Manager -> IO ()
spawnAction (Manager q _ _) = atomically $ writeTQueue q Spawn

addMyId :: Manager -> IO ()
addMyId (Manager q _ _) = do
    tid <- myThreadId
    atomically $ writeTQueue q $ Add tid

deleteMyId :: Manager -> IO ()
deleteMyId (Manager q _ _) = do
    tid <- myThreadId
    atomically $ writeTQueue q $ Delete tid

----------------------------------------------------------------

add :: ThreadId -> Set ThreadId -> Set ThreadId
add tid set = set'
  where
    !set' = Set.insert tid set

del :: ThreadId -> Set ThreadId -> Set ThreadId
del tid set = set'
  where
    !set' = Set.delete tid set

kill :: Set ThreadId -> IO ()
kill set = traverse_ killThread set

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
