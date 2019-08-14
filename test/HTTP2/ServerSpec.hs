{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module HTTP2.ServerSpec where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Crypto.Hash (Context, SHA1) -- cryptonite
import qualified Crypto.Hash as CH
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as C8
import Network.Run.TCP
import System.Exit
import System.Process.Typed
import Test.Hspec

import Network.HPACK
import Network.HPACK.Token
import Network.HTTP.Types
import Network.HTTP2.Server

spec :: Spec
spec = do
    describe "server" $ do
        it "passes the tests of h2spec" $ do
            tid <- forkIO echoServer
            runProcess (proc "h2spec" ["-h","127.0.0.1","-p","8080"]) `shouldReturn` ExitSuccess
            killThread tid

echoServer :: IO ()
echoServer = runTCPServer (Just "127.0.0.1") "8080" runHTTP2Server
  where
    runHTTP2Server s = E.bracket (allocSimpleConfig s 4096)
                                 (`run` server)
                                 freeSimpleConfig
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
