{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Exception as E
import Data.ByteString.Builder (byteString)
import Network.HTTP.Types (ok200)
import Network.Run.TCP (runTCPServer) -- network-run

import Network.HTTP2.Server

main :: IO ()
main = runTCPServer Nothing "80" $ \s _peer -> runHTTP2Server s
  where
    runHTTP2Server s = E.bracket (allocSimpleConfig s 4096)
                                 (\config -> run config server)
                                 freeSimpleConfig
    server _req _aux sendResponse = sendResponse response []
      where
        response = responseBuilder ok200 header body
        header = [("Content-Type", "text/plain")]
        body = byteString "Hello, world!\n"
