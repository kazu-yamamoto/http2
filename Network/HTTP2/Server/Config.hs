module Network.HTTP2.Server.Config where

import Data.IORef
import Foreign.Marshal.Alloc (mallocBytes, free)
import Network.Socket
import Network.Socket.ByteString (sendAll)

import Network.HPACK
import Network.HTTP2.Server.API
import Network.HTTP2.Server.File
import Network.HTTP2.Server.ReadN

-- | Making configuration whose IO is not efficient.
--   A write buffer is allocated internally.
--   That should be deallocated by the returned action.
makeSimpleConfig :: Socket -> BufferSize -> IO (Config, IO ())
makeSimpleConfig s bufsiz = do
    buf <- mallocBytes bufsiz
    ref <- newIORef Nothing
    let config = Config {
            confWriteBuffer = buf
          , confBufferSize = bufsiz
          , confSendAll = sendAll s
          , confReadN = defaultReadN s ref
          , confPositionReadMaker = defaultPositionReadMaker
          }
    return (config, free buf)
