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
    server _req sendResponse = sendResponse response []
      where
        response = responseBuilder ok200 [("Content-Type", "text/plain")] (byteString "Hello, world!\n")
    loop sock = forever $ do
        (s, _peer) <- accept sock
        (config, cleanup) <- makeSimpleConfig s 4096
        void $ forkFinally (run config server) (\_ -> close s >> cleanup)
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
