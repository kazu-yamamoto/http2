{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerSpec (spec) where

import Control.Concurrent
import qualified Control.Exception as E
import Data.ByteString.Builder (byteString)
import Network.HTTP.Types
import Network.Run.TCP
import System.Exit
import System.Process.Typed
import Test.Hspec

import Network.HTTP2.Server

port :: String
port = "8080"

host :: String
host = "127.0.0.1"

spec :: Spec
spec = do
    describe "server" $ do
        it "handles error cases" $
            E.bracket (forkIO runServer) killThread $ \_ -> do
                runProcess (proc "h2spec" ["-h", host, "-p", port]) `shouldReturn` ExitSuccess

runServer :: IO ()
runServer = runTCPServer (Just host) port runHTTP2Server
  where
    runHTTP2Server s =
        E.bracket
            (allocSimpleConfig s 4096)
            freeSimpleConfig
            (\conf -> run defaultServerConfig conf server)

server :: Server
server req _aux sendResponse = case requestMethod req of
    Just "GET" -> case requestPath req of
        Just "/" -> sendResponse responseHello []
        _ -> sendResponse response404 []
    _ -> sendResponse response405 []

responseHello :: Response
responseHello = responseBuilder ok200 header body
  where
    header = [("Content-Type", "text/plain")]
    body = byteString "Hello, world!\n"

response404 :: Response
response404 = responseNoBody notFound404 []

response405 :: Response
response405 = responseNoBody methodNotAllowed405 []
