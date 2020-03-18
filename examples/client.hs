{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Exception as E
import Control.Concurrent (forkIO)
import Data.ByteString ()
import Network.HTTP.Types
import Network.Run.TCP (runTCPClient) -- network-run

import Network.HTTP2.Client

authority :: String
authority = "127.0.0.1"

main :: IO ()
main = runTCPClient authority "80" $ runHTTP2Client
  where
    runHTTP2Client s = E.bracket (allocSimpleConfig s 4096)
                                 (`run` client)
                                 freeSimpleConfig
    client sendRequest = do
        let req = requestNoBody methodGet "/" []
        _ <- forkIO $ sendRequest req print
        sendRequest req print
