{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Client.Run where

import Data.IORef (writeIORef)
import UnliftIO.Async
import UnliftIO.Concurrent
import qualified UnliftIO.Exception as E
import UnliftIO.STM

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
            let runReceiver = frameReceiver ctx confReadN
                runSender = frameSender ctx conf mgr
            concurrently_ runReceiver runSender
    exchangeSettings conf ctx
    let runClient = do
            x <- client $ sendRequest ctx scheme authority
            let frame = goawayFrame 0 NoError "graceful closing"
            enqueueControl (controlQ ctx) $ CGoaway frame
            return x
    ex <- race runBackgroundThreads runClient `E.finally` stop mgr
    case ex of
      Left () -> undefined -- never reach
      Right x -> return x

sendRequest :: Context -> Scheme -> Authority -> Request -> (Response -> IO a) -> IO a
sendRequest ctx@Context{..} scheme auth (Request req) processResponse = do
    let hdr0 = outObjHeaders req
        method = fromMaybe (error "sendRequest:method") $ lookup ":method" hdr0
        path   = fromMaybe (error "sendRequest:path") $ lookup ":path" hdr0
    mstrm0 <- lookupCache method path roleInfo
    strm <- case mstrm0 of
      Nothing -> do
          let hdr1 | scheme /= "" = (":scheme", scheme) : hdr0
                   | otherwise    = hdr0
              hdr2 | auth /= "" = (":authority", auth) : hdr1
                   | otherwise  = hdr1
              req' = req { outObjHeaders = hdr2 }
          sid <- getMyNewStreamId ctx
          newstrm <- openStream ctx sid FrameHeaders
          case outObjBody req of
            OutBodyStreaming strmbdy -> do
                tbq <- newTBQueueIO 10 -- fixme: hard coding: 10
                enqueueOutput outputQ $ Output newstrm req' OObj (Just tbq) (return ())
                void $ forkIO $ do
                    let push b = atomically $ writeTBQueue tbq (StreamingBuilder b)
                        flush  = atomically $ writeTBQueue tbq StreamingFlush
                    strmbdy push flush
                    atomically $ writeTBQueue tbq StreamingFinished
            _ -> enqueueOutput outputQ $ Output newstrm req' OObj Nothing (return ())
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

