{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Exception as E
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import Network.Run.TCP (runTCPClient) -- network-run

import Network.HTTP2.Client

authority :: String
authority = "127.0.0.1"

main :: IO ()
main = runTCPClient authority "80" runHTTP2Client
  where
    runHTTP2Client s = E.bracket (allocSimpleConfig s 4096)
                                 freeSimpleConfig
                                 (\conf -> run conf "http" (C8.pack authority) client)
    client sendRequest = do
        let req = requestNoBody methodGet "/" []
        _ <- forkIO $ sendRequest req $ \rsp -> do
            print rsp
            getResponseBodyChunk rsp >>= C8.putStrLn
        sendRequest req $ \rsp -> do
            threadDelay 100000
            print rsp
            getResponseBodyChunk rsp >>= C8.putStrLn
