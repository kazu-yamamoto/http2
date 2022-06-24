{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Server.Run where

import UnliftIO.Async (race_)
import qualified UnliftIO.Exception as E

import Imports
import Network.HTTP2.Arch
import Network.HTTP2.Frame
import Network.HTTP2.Server.Types
import Network.HTTP2.Server.Worker

----------------------------------------------------------------

-- | Running HTTP/2 server.
run :: Config -> Server -> IO ()
run conf@Config{..} server = do
    ok <- checkPreface
    when ok $ do
        serverInfo <- newServerInfo
        ctx <- newContext serverInfo
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
        let runReceiver = frameReceiver ctx confReadN
            runSender = frameSender ctx conf mgr
        race_ runReceiver runSender `E.finally` stop mgr
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
