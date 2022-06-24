{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module HTTP2.ClientSpec where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Data.ByteString.Builder (byteString)
import Data.ByteString.Char8
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import Network.Run.TCP
import Network.Socket
import Test.Hspec

import Network.HPACK
import Network.HTTP2.Client
import qualified Network.HTTP2.Server as S

port :: String
port = "8080"

host :: String
host = "127.0.0.1"

spec :: Spec
spec = do
    describe "server" $ do
        it "receives an error in malformed cases" $
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                (runClient allocSimpleConfig)

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

runClient :: (Socket -> BufferSize -> IO Config) -> IO ()
runClient allocConfig =
  runTCPClient host port $ runHTTP2Client
  where
    auth = C8.pack host
    cliconf = ClientConfig "" auth 20
    runHTTP2Client s = E.bracket (allocConfig s 4096)
                                 freeSimpleConfig
                                 (\conf -> run cliconf conf client0 `shouldThrow` streamError)

streamError :: Selector HTTP2Error
streamError (StreamErrorIsReceived _ _) = True
streamError _                           = False

-- delay sending preface to be able to test if it is always sent first
allocSlowPrefaceConfig :: Socket -> BufferSize -> IO Config
allocSlowPrefaceConfig s size = do
  config <- allocSimpleConfig s size
  pure config { confSendAll = slowPrefaceSend (confSendAll config) }
  where
    slowPrefaceSend :: (ByteString -> IO ()) -> ByteString -> IO ()
    slowPrefaceSend orig chunk = do
      when (C8.pack "PRI" `isPrefixOf` chunk) $ do
        threadDelay 10000
      orig chunk

client0 :: Client ()
client0 sendRequest = do
    let req = requestNoBody methodGet "/" []
    sendRequest req $ \rsp -> do
        responseStatus rsp `shouldBe` Just ok200
        fmap statusMessage (responseStatus rsp) `shouldBe` Just "OK"
