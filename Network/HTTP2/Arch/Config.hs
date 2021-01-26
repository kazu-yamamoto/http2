module Network.HTTP2.Arch.Config where

import Data.ByteString (ByteString)
import Data.IORef
import Foreign.Marshal.Alloc (mallocBytes, free)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import qualified System.TimeManager as T

import Network.HPACK
import Network.HTTP2.Arch.File
import Network.HTTP2.Arch.ReadN

-- | HTTP/2 configuration.
data Config = Config {
    -- confWriteBuffer is used only by frameSender.
    -- This MUST be freed after frameSender is terminated.
      confWriteBuffer :: Buffer
    , confBufferSize  :: BufferSize
    , confSendAll     :: ByteString -> IO ()
    , confReadN       :: Int -> IO ByteString
    , confPositionReadMaker :: PositionReadMaker
    , confTimeoutManager :: T.Manager
    }

-- | Making simple configuration whose IO is not efficient.
--   A write buffer is allocated internally.
allocSimpleConfig :: Socket -> BufferSize -> IO Config
allocSimpleConfig s bufsiz = do
    buf <- mallocBytes bufsiz
    ref <- newIORef Nothing
    timmgr <- T.initialize $ 30 * 1000000
    let config = Config {
            confWriteBuffer = buf
          , confBufferSize = bufsiz
          , confSendAll = sendAll s
          , confReadN = defaultReadN s ref
          , confPositionReadMaker = defaultPositionReadMaker
          , confTimeoutManager = timmgr
          }
    return config

-- | Deallocating the resource of the simple configuration.
freeSimpleConfig :: Config -> IO ()
freeSimpleConfig conf = do
    free $ confWriteBuffer conf
    T.killManager $ confTimeoutManager conf
