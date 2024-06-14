{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Monad
import Crypto.Hash (Context, SHA1) -- cryptonite
import qualified Crypto.Hash as CH
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import Network.HTTP2.Server

server :: Server
server req _aux sendResponse = case requestMethod req of
    Just "GET" -> case requestPath req of
        Nothing -> sendResponse response404 []
        Just path
            | path == "/" -> sendResponse responseHello []
            | "/perf/" `B.isPrefixOf` path -> do
                case C8.readInt (B.drop 6 path) of
                    Nothing -> sendResponse responseHello []
                    Just (n, _) -> sendResponse (responsePerf n) []
            | otherwise -> sendResponse response404 []
    Just "POST" -> sendResponse (responseEcho req) []
    _ -> sendResponse response404 []

responseHello :: Response
responseHello = responseBuilder ok200 header body
  where
    header = [("Content-Type", "text/plain")]
    body = byteString "Hello, world!\n"

responsePerf :: Int -> Response
responsePerf n0 = responseStreaming ok200 header streaming
  where
    header = [("Content-Type", "text/plain")]
    bs1024 = BB.byteString $ B.replicate 1024 65
    streaming write _flush = loop n0
      where
        loop 0 = return ()
        loop n
            | n < 1024 = write $ BB.byteString $ B.replicate (fromIntegral n) 65
            | otherwise = do
                write bs1024
                loop (n - 1024)

response404 :: Response
response404 = responseBuilder notFound404 header body
  where
    header = [("Content-Type", "text/plain")]
    body = byteString "Not found\n"

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
    sha1 = C8.pack $ show $ CH.hashFinalize ctx
trailersMaker ctx (Just bs) = return $ NextTrailersMaker $ trailersMaker ctx'
  where
    ctx' = CH.hashUpdate ctx bs
