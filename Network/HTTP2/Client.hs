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
import Data.IORef (writeIORef)
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
    ctx <- newContext
    mgr <- start
    tid0 <- forkIO $ frameReceiver ctx confReadN
    tid1 <- forkIO $ clientAction $ sendRequest ctx
    -- enqueueControl controlQ setframe -- exchangeSettings conf ctx
    -- frameSender is the main thread because it ensures to send
    -- a goway frame.
    frameSender ctx conf mgr `E.finally` do
        clearContext ctx
        killThread tid0
        killThread tid1

sendRequest :: Context -> Client
sendRequest Context{..} req processResponse = do
    -- createStream
    let strm = undefined
    enqueueOutput outputQ $ Output strm req OObj Nothing (return ())
    rsp <- undefined
    processResponse rsp

exchangeSettings :: Config -> Context -> IO ()
exchangeSettings Config{..} Context{..} = do
    confSendAll connectionPreface
    -- sendFrame confSend id 0 initialSettingFrame
    writeIORef firstSettings True

initialSettingFrame :: FramePayload
initialSettingFrame = SettingsFrame [
    (SettingsMaxConcurrentStreams,recommendedConcurrency)
  ]

----------------------------------------------------------------

requestNoBody :: Method -> ByteString -> RequestHeaders -> Request
requestNoBody m p hdr = OutObj hdr' OutBodyNone defaultTrailersMaker
  where
    hdr' = (":method", m)
        : (":authority", "127.0.0.1") -- fixme
        : (":path", p)
        : (":scheme", "http") -- fixme
        : hdr
