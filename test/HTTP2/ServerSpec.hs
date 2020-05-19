{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module HTTP2.ServerSpec where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import Crypto.Hash (Context, SHA1) -- cryptonite
import qualified Crypto.Hash as CH
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)
import Data.ByteString.Char8
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import Network.Run.TCP
import System.Exit
import System.Process.Typed
import Test.Hspec

import Network.HPACK
import qualified Network.HTTP2.Client as C
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
                runProcess (proc "h2spec" ["-h",host,"-p",port]) `shouldReturn` ExitSuccess
        it "handles normal cases" $
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                runClient

runServer :: IO ()
runServer = runTCPServer (Just host) port runHTTP2Server
  where
    runHTTP2Server s = E.bracket (allocSimpleConfig s 4096)
                                 freeSimpleConfig
                                 (`run` server)

server :: Server
server req _aux sendResponse = case requestMethod req of
  Just "GET"  -> case requestPath req of
                   Just "/"     -> sendResponse responseHello []
                   Just "/push" -> do
                       let pp = pushPromise "/push-pp" responsePP defaultWeight
                       sendResponse responseHello [pp]
                   _            -> sendResponse response404 []
  Just "POST" -> case requestPath req of
                   Just "/echo" -> sendResponse (responseEcho req) []
                   _        -> sendResponse responseHello []
  _           -> sendResponse response405 []

responseHello :: Response
responseHello = responseBuilder ok200 header body
  where
    header = [("Content-Type", "text/plain")]
    body = byteString "Hello, world!\n"

responsePP :: Response
responsePP = responseBuilder ok200 header body
  where
    header = [("Content-Type", "text/plain")
             ,("x-push", "True")]
    body = byteString "Push\n"

response404 :: Response
response404 = responseNoBody notFound404 []

response405 :: Response
response405 = responseNoBody methodNotAllowed405 []

responseEcho :: Request -> Response
responseEcho req = setResponseTrailersMaker h2rsp maker
  where
    h2rsp = responseStreaming ok200 header streamingBody
    header = [("Content-Type", "text/plain")]
    streamingBody write _flush = do
        loop
        mt <- getRequestTrailers req
        firstTrailerValue <$> mt `shouldBe` Just "b0870457df2b8cae06a88657a198d9b52f8e2b0a"
      where
        loop = do
            bs <- getRequestBodyChunk req
            unless (B.null bs) $ do
                void $ write $ byteString bs
                loop
    maker = trailersMaker (CH.hashInit :: Context SHA1)

-- Strictness is important for Context.
trailersMaker :: Context SHA1 -> Maybe ByteString -> IO NextTrailersMaker
trailersMaker ctx Nothing = return $ Trailers [("X-SHA1", sha1)]
  where
    !sha1 = C8.pack $ show $ CH.hashFinalize ctx
trailersMaker ctx (Just bs) = return $ NextTrailersMaker $ trailersMaker ctx'
  where
    !ctx' = CH.hashUpdate ctx bs

runClient :: IO ()
runClient = runTCPClient host port $ runHTTP2Client
  where
    authority = C8.pack host
    runHTTP2Client s = E.bracket (allocSimpleConfig s 4096)
                                 freeSimpleConfig
                                 (\conf -> C.run conf "http" authority client)
    client sendRequest = mapConcurrently_ ($ sendRequest) clients
    clients = [client0,client1,client2,client3,client4]

client0 :: C.Client ()
client0 sendRequest = do
    let req = C.requestNoBody methodGet "/" []
    sendRequest req $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just ok200

client1 :: C.Client ()
client1 sendRequest = do
    let req = C.requestNoBody methodGet "/push-pp" []
    sendRequest req $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just notFound404

client2 :: C.Client ()
client2 sendRequest = do
    let req = C.requestNoBody methodPut "/" []
    sendRequest req $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just methodNotAllowed405

client3 :: C.Client ()
client3 sendRequest = do
    let req0 = C.requestFile methodPost "/echo" [] $ FileSpec "test/inputFile" 0 1012731
        req = C.setRequestTrailersMaker req0 maker
    sendRequest req $ \rsp -> do
        let comsumeBody = do
                bs <- C.getResponseBodyChunk rsp
                unless (B.null bs) comsumeBody
        comsumeBody
        mt <- C.getResponseTrailers rsp
        firstTrailerValue <$> mt `shouldBe` Just "b0870457df2b8cae06a88657a198d9b52f8e2b0a"
  where
    !maker = trailersMaker (CH.hashInit :: Context SHA1)

client4 :: C.Client ()
client4 sendRequest = do
    let req0 = C.requestNoBody methodGet "/push" []
    sendRequest req0 $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just ok200
    let req1 = C.requestNoBody methodGet "/push-pp" []
    sendRequest req1 $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just ok200

firstTrailerValue :: HeaderTable -> HeaderValue
firstTrailerValue = snd . Prelude.head . fst
