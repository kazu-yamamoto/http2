{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (forkFinally)
import Control.Exception (SomeException(..))
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Data.ByteString.Builder (byteString)
import Network.HTTP.Types (ok200)
import Network.HTTP2.Server
import Network.Socket

main :: IO ()
main = runTCPServer "80" $ \s _peer -> runHTTP2Server s
  where
    runHTTP2Server s = E.bracket (allocSimpleConfig s 4096)
                                 (\config -> run config server)
                                 freeSimpleConfig
    server _req _aux sendResponse = sendResponse response []
      where
        response = responseBuilder ok200 header body
        header = [("Content-Type", "text/plain")]
        body = byteString "Hello, world!\n"

runTCPServer :: String -> (Socket -> SockAddr -> IO a) -> IO a
runTCPServer port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) Nothing (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock $ setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ do
        (conn, peer) <- accept sock
        void $ forkFinally (server conn peer) (clear conn)
    clear conn _ = shutdown conn ShutdownSend `E.catch` ignore
      where
        ignore (SomeException _) = return ()
