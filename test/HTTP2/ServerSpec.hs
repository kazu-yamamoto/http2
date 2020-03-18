{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module HTTP2.ServerSpec where

import Control.Concurrent
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
import Network.HPACK.Token
import qualified Network.HTTP2.Client as C
import Network.HTTP2.Server

port :: String
port = "8080"

host :: String
host = "127.0.0.1"

spec :: Spec
spec = do
    describe "server" $ do
        it "passes the tests of h2spec" $ do
            tid <- forkIO echoServer
            runProcess (proc "h2spec" ["-h",host,"-p",port]) `shouldReturn` ExitSuccess
            client `shouldReturn` Just "da39a3ee5e6b4b0d3255bfef95601890afd80709"
            killThread tid

echoServer :: IO ()
echoServer = runTCPServer (Just host) port runHTTP2Server
  where
    runHTTP2Server s = E.bracket (allocSimpleConfig s 4096)
                                 freeSimpleConfig
                                 (`run` server)
    server req _aux sendResponse = case getHeaderValue tokenMethod vt of
      Just "GET"  -> sendResponse responseHello []
      Just "POST" -> sendResponse (responseEcho req) []
      _           -> sendResponse response404 []
      where
        (_, vt) = requestHeaders req

responseHello :: Response
responseHello = responseBuilder ok200 header body
  where
    header = [("Content-Type", "text/plain")]
    body = byteString "Hello, world!\n"

response404 :: Response
response404 = responseNoBody notFound404 []

responseEcho :: Request -> Response
responseEcho req = setResponseTrailersMaker h2rsp maker
  where
    h2rsp = responseStreaming ok200 header streamingBody
    header = [("Content-Type", "text/plain")]
    streamingBody write _flush = loop
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

client :: IO (Maybe HeaderValue)
client = runTCPClient host port $ runHTTP2Client
  where
    authority = C8.pack host
    runHTTP2Client s = E.bracket (allocSimpleConfig s 4096)
                                 freeSimpleConfig
                                 (\conf -> C.run conf "http" authority client')
    client' sendRequest = do
        let req = C.requestNoBody methodPost "/" []
        sendRequest req $ \rsp -> do
            _ <- C.getResponseBodyChunk rsp
            mht <- C.getResponseTrailers rsp
            return $ case mht of
                       Nothing     -> Nothing
                       Just (ht,_) -> Just (snd (Prelude.head ht))
