{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | HTTP\/2 client library.
--
--  Example:
--
-- > module Main where
-- >
-- > import Control.Concurrent (forkIO)
-- > import Data.ByteString.Char8 (pack)
-- > import Network.Run.TCP (runTCPClient) -- network-run
-- > import qualified Network.Socket.ByteString as NSB
-- >
-- > import Network.HTTP2.Client
-- >
-- > authority :: String
-- > authority = "127.0.0.1"
-- >
-- > main :: IO ()
-- > main = runTCPClient authority "80" $ \sock -> do
-- >     let conf = Config (NSB.sendAll sock) (NSB.recv sock)
-- >     run conf $ \client -> do
-- >         _ <- forkIO $ client defaultRequest{ requestAuthority = pack authority } print
-- >         client defaultRequest{ requestAuthority = pack authority } print

module Network.HTTP2.Client (
  -- * Connection
    run
  , Config(..)
  , allocSimpleConfig
  , freeSimpleConfig
  , Client
  -- * Stream
  , Request
  , Response
  , requestNoBody
  , OutObj
  , InpObj
  ) where

import Control.Concurrent
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import Data.IORef (readIORef,writeIORef)
import Network.HTTP.Types

import Network.HTTP2.Arch
import Network.HTTP2.Frame

----------------------------------------------------------------

-- | HTTP\/2 request for clients.
type Request = OutObj

-- | HTTP\/2 response for clients.
type Response = InpObj

type Client = Request -> (Response -> IO ()) -> IO ()

----------------------------------------------------------------

-- | Running HTTP/2 client.
run :: Config -> (Client -> IO ()) -> IO ()
run conf@Config{..} clientAction = do
    ctx <- newContext Client
    mgr <- start
    tid0 <- forkIO $ frameReceiver ctx confReadN
    exchangeSettings conf ctx
    -- frameSender is the main thread because it ensures to send
    -- a goway frame.
    tid1 <- forkIO $ do
        -- fixme: wait
        threadDelay 100000
        clientAction $ sendRequest ctx
    frameSender ctx conf mgr `E.finally` do
        clearContext ctx
        killThread tid0
        killThread tid1

sendRequest :: Context -> Client
sendRequest ctx@Context{..} req processResponse = do
    ws <- initialWindowSize <$> readIORef http2settings
    sid <- getMyNewStreamId ctx
    newstrm <- newStream sid (fromIntegral ws)
    opened ctx newstrm
    insert streamTable sid newstrm
    enqueueOutput outputQ $ Output newstrm req OObj Nothing (return ())
    rsp <- takeMVar $ streamInput newstrm
    processResponse rsp

exchangeSettings :: Config -> Context -> IO ()
exchangeSettings Config{..} Context{..} = do
    confSendAll connectionPreface
    let setframe = CSettings initialFrame [] -- fixme alist
    writeIORef firstSettings True
    enqueueControl controlQ setframe

----------------------------------------------------------------

requestNoBody :: Method -> ByteString -> RequestHeaders -> Request
requestNoBody m p hdr = OutObj hdr' OutBodyNone defaultTrailersMaker
  where
    hdr' = (":method", m)
        : (":authority", "127.0.0.1") -- fixme
        : (":path", p)
        : (":scheme", "http") -- fixme
        : hdr
