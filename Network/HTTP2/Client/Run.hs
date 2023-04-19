{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Client.Run where

import Control.Concurrent.STM (check)
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
    ctx <- newContext clientInfo confBufferSize
    mgr <- start confTimeoutManager
    let runBackgroundThreads = do
            let runReceiver = frameReceiver ctx conf
                runSender   = frameSender   ctx conf mgr
            concurrently_ runReceiver runSender
    exchangeSettings conf ctx
    let runClient = do
            x <- client $ sendRequest ctx mgr scheme authority
            let frame = goawayFrame 0 NoError "graceful closing"
            enqueueControl (controlQ ctx) $ CFrames Nothing [frame]
            return x
    ex <- race runBackgroundThreads runClient `E.finally` stop mgr
    case ex of
      Left () -> undefined -- never reach
      Right x -> return x

sendRequest :: Context -> Manager -> Scheme -> Authority -> Request -> (Response -> IO a) -> IO a
sendRequest ctx@Context{..} mgr scheme auth (Request req) processResponse = do
    -- Checking push promises
    let hdr0 = outObjHeaders req
        method = fromMaybe (error "sendRequest:method") $ lookup ":method" hdr0
        path   = fromMaybe (error "sendRequest:path") $ lookup ":path" hdr0
    mstrm0 <- lookupCache method path roleInfo
    strm <- case mstrm0 of
      Nothing -> do
          -- Arch/Sender is originally implemented for servers where
          -- the ordering of responses can be out-of-order.
          -- But for clients, the ordering must be maintained.
          -- To implement this, 'outputQStreamID' is used.
          -- Also, for 'OutBodyStreaming', TBQ must not be empty
          -- when its 'Output' is enqueued into 'outputQ'.
          -- Otherwise, it would be re-enqueue because of empty
          -- resulting in out-of-order.
          -- To implement this, 'tbqNonMmpty' is used.
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
                tbqNonMmpty <- newTVarIO False
                let setup = addMyId mgr
                let teardown _ = deleteMyId mgr
                E.bracket setup teardown $ \_ -> void $ forkIO $ do
                    let push b = atomically $ do
                            writeTBQueue tbq (StreamingBuilder b)
                            writeTVar tbqNonMmpty True
                        flush  = atomically $ writeTBQueue tbq StreamingFlush
                    strmbdy push flush
                    atomically $ writeTBQueue tbq StreamingFinished
                atomically $ do
                    sidOK <- readTVar outputQStreamID
                    ready <- readTVar tbqNonMmpty
                    check (sidOK == sid && ready)
                    writeTVar outputQStreamID (sid + 2)
                    writeTQueue outputQ $ Output newstrm req' OObj (Just tbq) (return ())
            _ -> atomically $ do
                sidOK <- readTVar outputQStreamID
                check (sidOK == sid)
                writeTVar outputQStreamID (sid + 2)
                writeTQueue outputQ $ Output newstrm req' OObj Nothing (return ())
          return newstrm
      Just strm0 -> return strm0
    rsp <- takeMVar $ streamInput strm
    processResponse $ Response rsp

exchangeSettings :: Config -> Context -> IO ()
exchangeSettings conf ctx@Context{..} = do
    frames <- updateMySettings conf ctx
    let setframe = CFrames Nothing (connectionPreface:frames)
    enqueueControl controlQ setframe
