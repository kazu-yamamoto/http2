{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Client.Run where

import Control.Concurrent
import qualified Control.Exception as E
import Data.IORef (writeIORef)

import Network.HTTP2.Arch
import Network.HTTP2.Client.Types
import Network.HTTP2.Frame

-- | Running HTTP/2 client.
run :: Config -> Scheme -> Authority -> Client a -> IO a
run conf@Config{..} scheme auth client = do
    ctx <- newContext Client
    mgr <- start
    tid0 <- forkIO $ frameReceiver ctx confReadN
    -- fixme: if frameSender is terminated but the main thread is alive,
    --        what will happen?
    tid1 <- forkIO $ frameSender ctx conf mgr
    exchangeSettings conf ctx
    client (sendRequest ctx scheme auth) `E.finally` do
        clearContext ctx
        stop mgr
        killThread tid0
        killThread tid1

sendRequest :: Context -> Scheme -> Authority -> Request -> (Response -> IO a) -> IO a
sendRequest ctx@Context{..} scheme auth (Request req) processResponse = do
    let hdr = outObjHeaders req
        hdr' = (":scheme", scheme)
             : (":authority", auth)
             : hdr
        req' = req { outObjHeaders = hdr' }
    sid <- getMyNewStreamId ctx
    newstrm <- openStream ctx sid FrameHeaders
    enqueueOutput outputQ $ Output newstrm req' OObj Nothing (return ())
    rsp <- takeMVar $ streamInput newstrm
    processResponse $ Response rsp

exchangeSettings :: Config -> Context -> IO ()
exchangeSettings Config{..} Context{..} = do
    confSendAll connectionPreface
    let setframe = CSettings initialFrame [] -- fixme alist
    writeIORef firstSettings True
    enqueueControl controlQ setframe

