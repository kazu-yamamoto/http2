{-# LANGUAGE NamedFieldPuns #-}

module Network.HTTP2.H2.Config where

import Control.Concurrent.STM
import Data.IORef
import Foreign.Marshal.Alloc (free, mallocBytes)
import Network.HTTP.Semantics.Client
import Network.Socket
import Network.Socket.ByteString (sendAll)

import Network.HPACK
import Network.HTTP2.H2.Types

-- | Making simple configuration whose IO is not efficient.
--   A write buffer is allocated internally.
--   The timeout of the connection is 30_000_000 microseconds.
allocSimpleConfig :: Socket -> BufferSize -> IO Config
allocSimpleConfig s bufsiz = allocSimpleConfig' s bufsiz (30 * 1000000)

-- | Making simple configuration whose IO is not efficient.
--   A write buffer is allocated internally.
--   The third argument is the timeout of the connection in
--   microseconds. Zero or less means no timeout.
allocSimpleConfig' :: Socket -> BufferSize -> Int -> IO Config
allocSimpleConfig' s bufsiz usec = allocSimpleConfigWith s bufsiz usec retry

-- | 'allocSimpleConfig'' whose receive the caller can give up on.  The
--   'STM' action is waited for beside each receive; when it fires, the
--   reader ends as it does at end of input.  Nothing is closed and no
--   thread is interrupted to make it return.
--
--   @since 5.4.11
allocSimpleConfigWith :: Socket -> BufferSize -> Int -> STM a -> IO Config
allocSimpleConfigWith s bufsiz usec giveUp = do
    confWriteBuffer <- mallocBytes bufsiz
    let confBufferSize = bufsiz
    let confSendAll = sendAll s
    confReadN <- (\ref -> defaultReadNWith s ref giveUp) <$> newIORef Nothing
    let confPositionReadMaker = defaultPositionReadMaker
    let confTimeout = usec
    confMySockAddr <- getSocketName s
    confPeerSockAddr <- getPeerName s
    let confOnInformational = \_ _ -> return ()
    -- The deprecated fields are left as in 'defaultConfig'.
    return
        defaultConfig
            { confWriteBuffer
            , confBufferSize
            , confSendAll
            , confReadN
            , confPositionReadMaker
            , confTimeout
            , confMySockAddr
            , confPeerSockAddr
            , confOnInformational
            }

-- | Deallocating the resource of the simple configuration.
--
--   This does not cancel timeout actions registered with
--   'confTimeoutManager'.  Since time-manager 0.3, a manager holds no
--   registrations, so there is nothing to kill: an action registered with
--   'System.TimeManager.register' runs even after this returns unless it
--   is cancelled with 'System.TimeManager.cancel'.  Use
--   'System.TimeManager.withHandle', which cancels it when the scope ends.
freeSimpleConfig :: Config -> IO ()
freeSimpleConfig conf = free $ confWriteBuffer conf
