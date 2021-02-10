{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Arch.Queue where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (bracket)

import Imports
import Network.HTTP2.Arch.Manager
import Network.HTTP2.Arch.Types

{-# INLINE forkAndEnqueueWhenReady #-}
forkAndEnqueueWhenReady :: IO () -> TQueue (Output Stream) -> Output Stream -> Manager -> IO ()
forkAndEnqueueWhenReady wait outQ out mgr = bracket setup teardown $ \_ ->
    void . forkIO $ do
        wait
        enqueueOutput outQ out
  where
    setup = addMyId mgr
    teardown _ = deleteMyId mgr

{-# INLINE enqueueOutput #-}
enqueueOutput :: TQueue (Output Stream) -> Output Stream -> IO ()
enqueueOutput outQ out = atomically $ writeTQueue outQ out

{-# INLINE enqueueControl #-}
enqueueControl :: TQueue Control -> Control -> IO ()
enqueueControl ctlQ ctl = atomically $ writeTQueue ctlQ ctl

----------------------------------------------------------------
