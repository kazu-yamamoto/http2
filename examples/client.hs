{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO)
import Data.ByteString ()
import Network.HTTP.Types
import Network.Run.TCP (runTCPClient) -- network-run
import qualified Network.Socket.ByteString as NSB

import Network.HTTP2.Client

authority :: String
authority = "127.0.0.1"

main :: IO ()
main = runTCPClient authority "80" $ \sock -> do
    let conf = Config (NSB.sendAll sock) (NSB.recv sock)
    run conf $ \client -> do
        _ <- forkIO $ client (requestNoBody methodGet "/" []) print
        client (requestNoBody methodGet "/" []) print
