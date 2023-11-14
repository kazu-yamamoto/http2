{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Queue where

import UnliftIO.STM

import Network.HTTP2.H2.Manager
import Network.HTTP2.H2.Types

{-# INLINE forkAndEnqueueWhenReady #-}
forkAndEnqueueWhenReady
    :: IO () -> TQueue (Output Stream) -> Output Stream -> Manager -> IO ()
forkAndEnqueueWhenReady wait outQ out mgr =
    forkManaged mgr $ do
        wait
        enqueueOutput outQ out

{-# INLINE enqueueOutput #-}
enqueueOutput :: TQueue (Output Stream) -> Output Stream -> IO ()
enqueueOutput outQ out = atomically $ writeTQueue outQ out

{-# INLINE enqueueControl #-}
enqueueControl :: TQueue Control -> Control -> IO ()
enqueueControl ctlQ ctl = atomically $ writeTQueue ctlQ ctl

----------------------------------------------------------------
