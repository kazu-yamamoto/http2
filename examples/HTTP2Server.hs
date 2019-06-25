{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Data.ByteString.Builder (byteString)
import Network.HTTP.Types (ok200)
import Network.HTTP2.Server
import Network.Socket

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve "80"
    E.bracket (open addr) close loop
  where
    server _req _aux sendResponse = sendResponse response []
      where
        response = responseBuilder ok200 [("Content-Type", "text/plain")] (byteString "Hello, world!\n")
    runHTTP2Server s = E.bracket (allocSimpleConfig s 4096)
                                 (\config -> run config server)
                                 freeSimpleConfig
    loop sock = forever $ do
        (s, _peer) <- accept sock
        void $ forkFinally (runHTTP2Server s) (\_ -> close s)
    resolve port = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock $ setCloseOnExecIfNeeded
        bind sock (addrAddress addr)
        listen sock 10
        return sock
