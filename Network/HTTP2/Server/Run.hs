{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Server.Run where

import UnliftIO.Async (concurrently_)

import Imports
import Network.HTTP2.Arch
import Network.HTTP2.Frame
import Network.HTTP2.Server.Types
import Network.HTTP2.Server.Worker
import Control.Exception

----------------------------------------------------------------

-- | Running HTTP/2 server.
run :: Config -> Server -> IO ()
run conf@Config{..} server = do
    ok <- checkPreface
    when ok $ do
        serverInfo <- newServerInfo
        ctx <- newContext serverInfo confBufferSize confMySockAddr confPeerSockAddr
        -- Workers, worker manager and timer manager
        mgr <- start confTimeoutManager
        let wc = fromContext ctx
        setAction mgr $ worker wc mgr server
        -- The number of workers is 3.
        -- This was carefully chosen based on a lot of benchmarks.
        -- If it is 1, we cannot avoid head-of-line blocking.
        -- If it is large, huge memory is consumed and many
        -- context switches happen.
        replicateM_ 3 $ spawnAction mgr
        let runReceiver = frameReceiver ctx conf
            runSender   = frameSender   ctx conf mgr
        stopAfter mgr (concurrently_ runReceiver runSender) $ \res -> do
          closeAllStreams (streamTable ctx) $ either Just (const Nothing) res
          case res of
            Left err ->
              throwIO err
            Right x ->
              return x
  where
    checkPreface = do
        preface <- confReadN connectionPrefaceLength
        if connectionPreface /= preface then do
            goaway conf ProtocolError "Preface mismatch"
            return False
          else
            return True

-- connClose must not be called here since Run:fork calls it
goaway :: Config -> ErrorCode -> ByteString -> IO ()
goaway Config{..} etype debugmsg = confSendAll bytestream
  where
    bytestream = goawayFrame 0 etype debugmsg
