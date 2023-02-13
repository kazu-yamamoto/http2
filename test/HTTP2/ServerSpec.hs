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
import Data.ByteString.Builder (byteString, Builder)
import Data.ByteString.Char8
import qualified Data.ByteString.Char8 as C8
import Data.IORef
import Network.HTTP.Types
import Network.Run.TCP
import Network.Socket
import Network.Socket.ByteString
import Test.Hspec
import System.IO

import Network.HPACK
import qualified Network.HTTP2.Client as C
import Network.HTTP2.Server
import Network.HTTP2.Frame

port :: String
port = "8080"

host :: String
host = "127.0.0.1"

spec :: Spec
spec = do
    describe "server" $ do
        it "handles normal cases" $
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                (runClient allocSimpleConfig)
        it "should always send the connection preface first" $ do
            prefaceVar <- newEmptyMVar
            E.bracket (forkIO (runFakeServer prefaceVar)) killThread $ \_ -> do
                threadDelay 10000
                E.catch (runClient allocSlowPrefaceConfig) ignoreHTTP2Error

            preface <- takeMVar prefaceVar
            preface `shouldBe` connectionPreface

ignoreHTTP2Error :: C.HTTP2Error -> IO ()
ignoreHTTP2Error _ = pure ()

runServer :: IO ()
runServer = runTCPServer (Just host) port runHTTP2Server
  where
    runHTTP2Server s = E.bracket (allocSimpleConfig s 4096)
                                 freeSimpleConfig
                                 (`run` server)

runFakeServer :: MVar ByteString -> IO ()
runFakeServer prefaceVar = do
  runTCPServer (Just host) port $ \s -> do
    ref <- newIORef Nothing

    -- send settings
    sendAll s $ "\x00\x00\x12\x04\x00\x00\x00\x00\x00"
      `mappend` "\x00\x03\x00\x00\x00\x80\x00\x04\x00"
      `mappend` "\x01\x00\x00\x00\x05\x00\xff\xff\xff"

    -- receive preface
    value <- defaultReadN s ref (B.length connectionPreface)
    putMVar prefaceVar value

    -- send goaway frame
    sendAll s "\x00\x00\x08\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01"

    -- wait for a few ms to make sure the client has a chance to close the
    -- socket on its end
    threadDelay 10000

server :: Server
server req _aux sendResponse = case requestMethod req of
  Just "GET"  -> case requestPath req of
                   Just "/"     -> sendResponse responseHello []
                   Just "/stream" -> sendResponse responseInfinite []
                   Just "/push" -> do
                       let pp = pushPromise "/push-pp" responsePP 0
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

responseInfinite :: Response
responseInfinite = responseStreaming ok200 header body
  where
    header = [("Content-Type", "text/plain")]
    body :: (Builder -> IO ()) -> IO () -> IO ()
    body write flush = do
      let go n = write (byteString (C8.pack (show n)) `mappend` "\n") *> flush *> go (succ n)
      go (0 :: Int)

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

runClient :: (Socket -> BufferSize -> IO Config) -> IO ()
runClient allocConfig =
  runTCPClient host port $ runHTTP2Client
  where
    authority = C8.pack host
    cliconf = C.ClientConfig "http" authority 20
    runHTTP2Client s = E.bracket (allocConfig s 4096)
                                 freeSimpleConfig
                                 (\conf -> C.run cliconf conf client)
    client sendRequest = mapConcurrently_ ($ sendRequest) clients
    clients = [client0,client1,client2,client3,client3',client4,client5]

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

client0 :: C.Client ()
client0 sendRequest = do
    let req = C.requestNoBody methodGet "/" []
    sendRequest req $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just ok200
        fmap statusMessage (C.responseStatus rsp) `shouldBe` Just "OK"

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

client3' :: C.Client ()
client3' sendRequest = do
    let req0 = C.requestStreaming methodPost "/echo" [] $ \sendChunk flush -> do
            let sendFile h = do
                    bs <- B.hGet h 1024
                    when (bs /= "") $ do
                        sendChunk $ byteString bs
                        sendFile h
            withFile "test/inputFile" ReadMode sendFile
            flush
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

client5 :: C.Client ()
client5 sendRequest = do
    let req0 = C.requestNoBody methodGet "/stream" []
    sendRequest req0 $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just ok200
        let go n | n > 0 = do _ <- C.getResponseBodyChunk rsp
                              go (pred n)
                 | otherwise = pure ()
        go (100 :: Int)

firstTrailerValue :: HeaderTable -> HeaderValue
firstTrailerValue = snd . Prelude.head . fst
