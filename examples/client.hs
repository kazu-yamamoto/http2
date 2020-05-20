{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Exception as E
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import Network.Run.TCP (runTCPClient) -- network-run

import Network.HTTP2.Client

serverName :: String
serverName = "127.0.0.1"

main :: IO ()
main = runTCPClient serverName "80" runHTTP2Client
  where
    cliconf = ClientConfig "http" (C8.pack serverName) 20
    runHTTP2Client s = E.bracket (allocSimpleConfig s 4096)
                                 freeSimpleConfig
                                 (\conf -> run cliconf conf client)
    client sendRequest = do
        let req = requestNoBody methodGet "/" []
        _ <- forkIO $ sendRequest req $ \rsp -> do
            print rsp
            getResponseBodyChunk rsp >>= C8.putStrLn
        sendRequest req $ \rsp -> do
            threadDelay 100000
            print rsp
            getResponseBodyChunk rsp >>= C8.putStrLn
