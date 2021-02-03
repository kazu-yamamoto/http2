{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import Network.HTTP2.Client
import Network.Run.TCP (runTCPClient) -- network-run
import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 2) $ do
        putStrLn "client <addr> <port>"
        exitFailure
    let [host,port] = args
    runTCPClient host port $ runHTTP2Client host
  where
    cliconf host = ClientConfig "http" (C8.pack host) 20
    runHTTP2Client host s = E.bracket (allocSimpleConfig s 4096)
                                      freeSimpleConfig
                                      (\conf -> run (cliconf host) conf client)
    client sendRequest = do
        let req = requestNoBody methodGet "/" []
        _ <- forkIO $ sendRequest req $ \rsp -> do
            print rsp
            getResponseBodyChunk rsp >>= C8.putStrLn
        sendRequest req $ \rsp -> do
            threadDelay 100000
            print rsp
            getResponseBodyChunk rsp >>= C8.putStrLn
