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
    -- | This is used only by frameSender.
    -- This MUST be freed after frameSender is terminated.
      confWriteBuffer :: Buffer
    -- | The size of the write buffer.
    --   We assume that the read buffer is the same size.
    --   So, this value is announced via SETTINGS_MAX_FRAME_SIZE
    --   to the peer.
    , confBufferSize  :: BufferSize
    , confSendAll     :: ByteString -> IO ()
    , confReadN       :: Int -> IO ByteString
    , confPositionReadMaker :: PositionReadMaker
    , confTimeoutManager :: T.Manager
    -- | This is copied into 'Aux', if exist, on server.
    , confMySockAddr     :: SockAddr
    -- | This is copied into 'Aux', if exist, on server.
    , confPeerSockAddr   :: SockAddr
    }

-- | Making simple configuration whose IO is not efficient.
--   A write buffer is allocated internally.
allocSimpleConfig :: Socket -> BufferSize -> IO Config
allocSimpleConfig s bufsiz = do
    buf <- mallocBytes bufsiz
    ref <- newIORef Nothing
    timmgr <- T.initialize $ 30 * 1000000
    mysa <- getSocketName s
    peersa <- getPeerName s
    let config = Config {
            confWriteBuffer = buf
          , confBufferSize = bufsiz
          , confSendAll = sendAll s
          , confReadN = defaultReadN s ref
          , confPositionReadMaker = defaultPositionReadMaker
          , confTimeoutManager = timmgr
          , confMySockAddr   = mysa
          , confPeerSockAddr = peersa
          }
    return config

-- | Deallocating the resource of the simple configuration.
freeSimpleConfig :: Config -> IO ()
freeSimpleConfig conf = do
    free $ confWriteBuffer conf
    T.killManager $ confTimeoutManager conf
