module Network.HTTP2.H2.Config where

import Data.IORef
import Foreign.Marshal.Alloc (free, mallocBytes)
import Network.HTTP.Semantics.Client
import Network.Socket
import Network.Socket.ByteString (sendAll)

import Network.HPACK
import Network.HTTP2.H2.Types

-- | Making simple configuration whose IO is not efficient.
--   A write buffer is allocated internally.
allocSimpleConfig :: Socket -> BufferSize -> IO Config
allocSimpleConfig s bufsiz = do
    buf <- mallocBytes bufsiz
    ref <- newIORef Nothing
    mysa <- getSocketName s
    peersa <- getPeerName s
    let config =
            Config
                { confWriteBuffer = buf
                , confBufferSize = bufsiz
                , confSendAll = sendAll s
                , confReadN = defaultReadN s ref
                , confPositionReadMaker = defaultPositionReadMaker
                , confTimeout = 30000000
                , confMySockAddr = mysa
                , confPeerSockAddr = peersa
                }
    return config

-- | Deallocating the resource of the simple configuration.
freeSimpleConfig :: Config -> IO ()
freeSimpleConfig conf = free $ confWriteBuffer conf
