{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Queue where

import Control.Concurrent.STM

import Network.HTTP2.H2.Types

{-# INLINE enqueueOutput #-}
enqueueOutput :: TQueue Output -> Output -> IO ()
enqueueOutput outQ out = atomically $ writeTQueue outQ out

{-# INLINE enqueueOutputSTM #-}
enqueueOutputSTM :: TQueue Output -> Output -> STM ()
enqueueOutputSTM outQ out = writeTQueue outQ out

{-# INLINE enqueueControl #-}
enqueueControl :: TQueue Control -> Control -> IO ()
enqueueControl ctlQ ctl = atomically $ writeTQueue ctlQ ctl

----------------------------------------------------------------
