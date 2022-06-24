{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import Network.Run.TCP (runTCPClient) -- network-run

import Network.HTTP2.Client

serverName :: String
serverName = "127.0.0.1"

main :: IO ()
main = runTCPClient serverName "80" $ runHTTP2Client serverName
  where
    cliconf host = ClientConfig "http" (C8.pack host) 20
    runHTTP2Client host s = E.bracket (allocSimpleConfig s 4096)
                                      freeSimpleConfig
                                      (\conf -> run (cliconf host) conf client)
    client sendRequest = do
        let req0 = requestNoBody methodGet "/" []
            client0 = sendRequest req0 $ \rsp -> do
                print rsp
                getResponseBodyChunk rsp >>= C8.putStrLn
            req1 = requestNoBody methodGet "/foo" []
            client1 = sendRequest req1 $ \rsp -> do
                print rsp
                getResponseBodyChunk rsp >>= C8.putStrLn
        ex <- E.try $ concurrently_ client0 client1
        case ex of
          Left  e  -> print (e :: HTTP2Error)
          Right () -> putStrLn "OK"
