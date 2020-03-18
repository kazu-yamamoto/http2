module Network.HTTP2.Arch.Config where

import Data.ByteString (ByteString)
import Data.IORef
import Foreign.Marshal.Alloc (mallocBytes, free)
import Network.Socket
import Network.Socket.ByteString (sendAll)

import Network.HPACK
import Network.HTTP2.Arch.File
import Network.HTTP2.Arch.ReadN

-- | HTTP/2 server configuration.
data Config = Config {
      confWriteBuffer :: Buffer
    , confBufferSize  :: BufferSize
    , confSendAll     :: ByteString -> IO ()
    , confReadN       :: Int -> IO ByteString
    , confPositionReadMaker :: PositionReadMaker
    }

{-# DEPRECATED makeSimpleConfig "Use allocSimpleConfig instead" #-}
-- | Making configuration whose IO is not efficient.
--   A write buffer is allocated internally.
--   That should be deallocated by the returned action.
makeSimpleConfig :: Socket -> BufferSize -> IO (Config, IO ())
makeSimpleConfig s bufsiz = do
    config <- allocSimpleConfig s bufsiz
    return (config, freeSimpleConfig config)

-- | Making simple configuration whose IO is not efficient.
--   A write buffer is allocated internally.
allocSimpleConfig :: Socket -> BufferSize -> IO Config
allocSimpleConfig s bufsiz = do
    buf <- mallocBytes bufsiz
    ref <- newIORef Nothing
    let config = Config {
            confWriteBuffer = buf
          , confBufferSize = bufsiz
          , confSendAll = sendAll s
          , confReadN = defaultReadN s ref
          , confPositionReadMaker = defaultPositionReadMaker
          }
    return config

-- | Deallocating the resource of the simple configuration.
freeSimpleConfig :: Config -> IO ()
freeSimpleConfig conf = free $ confWriteBuffer conf
