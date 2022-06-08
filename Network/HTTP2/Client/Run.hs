{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Client.Run where

import Control.Concurrent.Async
import Control.Concurrent
import qualified Control.Exception as E
import Data.IORef (writeIORef)

import Imports
import Network.HTTP2.Arch
import Network.HTTP2.Client.Types
import Network.HTTP2.Frame

-- | Client configuration
data ClientConfig = ClientConfig {
    scheme     :: Scheme    -- ^ https or http
  , authority  :: Authority -- ^ Server name
  , cacheLimit :: Int       -- ^ How many pushed responses are contained in the cache
  }

-- | Running HTTP/2 client.
run :: ClientConfig -> Config -> Client a -> IO a
run ClientConfig{..} conf@Config{..} client = do
    clientInfo <- newClientInfo scheme authority cacheLimit
    ctx <- newContext clientInfo
    mgr <- start confTimeoutManager
    let runBackgroundThreads = do
            race_
                (frameReceiver ctx confReadN)
                (frameSender ctx conf mgr)
            E.throwIO (ConnectionError ProtocolError "connection terminated")
    exchangeSettings conf ctx
    fmap (either id id) $
        race runBackgroundThreads (client (sendRequest ctx scheme authority))
            `E.finally` stop mgr

sendRequest :: Context -> Scheme -> Authority -> Request -> (Response -> IO a) -> IO a
sendRequest ctx@Context{..} scheme auth (Request req) processResponse = do
    let hdr = outObjHeaders req
        method = fromMaybe (error "sendRequest:method") $ lookup ":method" hdr
        path   = fromMaybe (error "sendRequest:path") $ lookup ":path" hdr
    mstrm0 <- lookupCache method path roleInfo
    strm <- case mstrm0 of
      Nothing -> do
          let hdr' = (":scheme", scheme)
                   : (":authority", auth)
                   : hdr
              req' = req { outObjHeaders = hdr' }
          sid <- getMyNewStreamId ctx
          newstrm <- openStream ctx sid FrameHeaders
          enqueueOutput outputQ $ Output newstrm req' OObj Nothing (return ())
          return newstrm
      Just strm0 -> return strm0
    rsp <- takeMVar $ streamInput strm
    processResponse $ Response rsp

exchangeSettings :: Config -> Context -> IO ()
exchangeSettings Config{..} Context{..} = do
    confSendAll connectionPreface
    let setframe = CSettings initialFrame [] -- fixme alist
    writeIORef firstSettings True
    enqueueControl controlQ setframe

