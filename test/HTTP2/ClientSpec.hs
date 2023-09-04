{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module HTTP2.ClientSpec where

import Control.Concurrent
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import Network.Run.TCP
import System.IO.Unsafe (unsafePerformIO)
import System.Random
import Test.Hspec

import Network.HTTP2.Client
import qualified Network.HTTP2.Server as S

port :: String
port = show $ unsafePerformIO (randomPort <$> getStdGen)
  where
    randomPort = fst . randomR (43124 :: Int, 44320)

host :: String
host = "127.0.0.1"

host' :: ByteString
host' = C8.pack host

spec :: Spec
spec = do
    describe "client" $ do
        it "receives an error if scheme is missing" $
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                runClient "" host' [] `shouldThrow` streamError

        it "receives an error if authority is missing" $
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                runClient "http" "" [] `shouldThrow` streamError

        it "receives an error if authority and host are different" $
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                runClient "http" host' [("Host","foo")] `shouldThrow` streamError

runServer :: IO ()
runServer = runTCPServer (Just host) port runHTTP2Server
  where
    runHTTP2Server s = E.bracket (allocSimpleConfig s 4096)
                                 freeSimpleConfig
                                 (`S.run` server)
server :: S.Server
server _req _aux sendResponse = sendResponse responseHello []

responseHello :: S.Response
responseHello = S.responseBuilder ok200 header body
  where
    header = [("Content-Type", "text/plain")]
    body = byteString "Hello, world!\n"

runClient :: Scheme -> Authority -> RequestHeaders -> IO ()
runClient sc au hd = runTCPClient host port $ runHTTP2Client
  where
    cliconf = ClientConfig sc au 20
    runHTTP2Client s = E.bracket (allocSimpleConfig s 4096)
                                 freeSimpleConfig
                                 (\conf -> run cliconf conf $ \sendRequest ->
                                   client sendRequest)
    client sendRequest = do
        let req = requestNoBody methodGet "/" hd
        sendRequest req $ \rsp -> do
            responseStatus rsp `shouldBe` Just ok200
            fmap statusMessage (responseStatus rsp) `shouldBe` Just "OK"

streamError :: Selector HTTP2Error
streamError (StreamErrorIsReceived _ _) = True
streamError _                           = False
