{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Arch.Queue where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (bracket)
import Data.IORef

import Imports
import Network.HTTP2.Arch.Manager
import Network.HTTP2.Arch.Types
import Network.HTTP2.Priority

{-# INLINE forkAndEnqueueWhenReady #-}
forkAndEnqueueWhenReady :: IO () -> PriorityTree Output -> Output -> Manager -> IO ()
forkAndEnqueueWhenReady wait outQ out mgr = bracket setup teardown $ \_ ->
    void . forkIO $ do
        wait
        enqueueOutput outQ out
  where
    setup = addMyId mgr
    teardown _ = deleteMyId mgr

{-# INLINE enqueueOutput #-}
enqueueOutput :: PriorityTree Output -> Output -> IO ()
enqueueOutput outQ out = do
    let Stream{..} = outputStream out
    pre <- readIORef streamPrecedence
    enqueue outQ streamNumber pre out

{-# INLINE enqueueControl #-}
enqueueControl :: TQueue Control -> Control -> IO ()
enqueueControl ctlQ ctl = atomically $ writeTQueue ctlQ ctl

----------------------------------------------------------------
