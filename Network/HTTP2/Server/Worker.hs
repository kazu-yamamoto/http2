{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Server.Worker (
    worker
  , WorkerConf(..)
  , fromContext
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (SomeException(..), AsyncException(..))
import qualified Control.Exception as E
import Data.IORef
import qualified Network.HTTP.Types as H
import Network.Wai (StreamingBody)
import qualified System.TimeManager as T

import Imports hiding (insert)
import Network.HPACK
import Network.HPACK.Token
import Network.HTTP2.Arch
import Network.HTTP2.Frame
import Network.HTTP2.Priority
import Network.HTTP2.Server.Types

----------------------------------------------------------------

data WorkerConf a = WorkerConf {
    readInputQ     :: IO (Input a)
  , writeOutputQ   :: Output a -> IO ()
  , workerCleanup  :: a -> IO ()
  , isPushable     :: IO Bool
  , insertStream   :: StreamId -> a -> IO ()
  , makePushStream :: a -> PushPromise -> IO (StreamId, StreamId, a)
  }

fromContext :: Context -> WorkerConf Stream
fromContext ctx@Context{..} = WorkerConf {
    readInputQ = atomically $ readTQueue $ inputQ roleInfo
  , writeOutputQ = enqueueOutput outputQ
  , workerCleanup = \strm -> do
        closed ctx strm Killed
        let frame = resetFrame InternalError (streamNumber strm)
        enqueueControl controlQ $ CFrame frame
  , isPushable = enablePush <$> readIORef http2settings
  , insertStream = insert streamTable
  , makePushStream = \pstrm pp -> do
        ws <- initialWindowSize <$> readIORef http2settings
        let w = promiseWeight pp
            pri = defaultPriority { weight = w }
            pre = toPrecedence pri
        sid <- getMyNewStreamId ctx
        newstrm <- newPushStream sid ws pre
        let pid = streamNumber pstrm
        return (pid, sid, newstrm)
  }

----------------------------------------------------------------

pushStream :: WorkerConf a
           -> a -- parent stream
           -> ValueTable -- request
           -> [PushPromise]
           -> IO OutputType
pushStream _ _ _ [] = return OObj
pushStream WorkerConf{..} pstrm reqvt pps0
  | len == 0 = return OObj
  | otherwise = do
        pushable <- isPushable
        if pushable then do
            tvar <- newTVarIO 0
            lim <- push tvar pps0 0
            if lim == 0 then
              return OObj
             else
              return $ OWait (waiter lim tvar)
          else
            return OObj
  where
    len = length pps0
    increment tvar = atomically $ modifyTVar' tvar (+1)
    waiter lim tvar = atomically $ do
        n <- readTVar tvar
        check (n >= lim)
    push _ [] n = return (n :: Int)
    push tvar (pp:pps) n = do
        (pid, sid, newstrm) <- makePushStream pstrm pp
        insertStream sid newstrm
        let scheme = fromJust $ getHeaderValue tokenScheme reqvt
            -- fixme: this value can be Nothing
            auth   = fromJust (getHeaderValue tokenHost reqvt
                            <|> getHeaderValue tokenAuthority reqvt)
            path = promiseRequestPath pp
            promiseRequest = [(tokenMethod, H.methodGet)
                             ,(tokenScheme, scheme)
                             ,(tokenAuthority, auth)
                             ,(tokenPath, path)]
            ot = OPush promiseRequest pid
            Response rsp = promiseResponse pp
            out = Output newstrm rsp ot Nothing $ increment tvar
        writeOutputQ out
        push tvar pps (n + 1)

-- | This function is passed to workers.
--   They also pass 'Response's from a server to this function.
--   This function enqueues commands for the HTTP/2 sender.
response :: WorkerConf a -> Manager -> a -> Request -> Response -> [PushPromise] -> IO ()
response wc@WorkerConf{..} mgr strm (Request req) (Response rsp) pps = case outObjBody rsp of
  OutBodyNone -> do
      writeOutputQ $ Output strm rsp OObj Nothing (return ())
  OutBodyBuilder _ -> do
      otyp <- pushStream wc strm reqvt pps
      writeOutputQ $ Output strm rsp otyp Nothing (return ())
  OutBodyFile _ -> do
      otyp <- pushStream wc strm reqvt pps
      writeOutputQ $ Output strm rsp otyp Nothing (return ())
  OutBodyStreaming strmbdy -> do
      otyp <- pushStream wc strm reqvt pps
      tbq <- newTBQueueIO 10 -- fixme: hard coding: 10
      writeOutputQ $ Output strm rsp otyp (Just tbq) (return ())
      void $ forkIO (responseStreaming mgr tbq strmbdy `E.catch` cleanup wc strm)
  where
    (_,reqvt) = inpObjHeaders req

responseStreaming :: Manager -> TBQueue StreamingChunk -> StreamingBody -> IO ()
responseStreaming mgr tbq strmbdy = timeoutKillThread mgr $ \nth -> do
    strmbdy (push nth) flush
    atomically $ writeTBQueue tbq StreamingFinished
    deleteMyId mgr
  where
    push nth b = do
        T.pause nth
        atomically $ writeTBQueue tbq (StreamingBuilder b)
        T.resume nth
    flush = atomically $ writeTBQueue tbq StreamingFlush

-- | Worker for server applications.
worker :: WorkerConf a -> Manager -> Server -> Action
worker wc@WorkerConf{..} mgr server = timeoutKillThread mgr $ \th -> forever $ do
    T.pause th
    Input strm inp <- readInputQ
    T.resume th
    T.tickle th
    let req = pauseRequestBody inp th
        aux = Aux th
        sendRsp = response wc mgr strm req
    server req aux sendRsp `E.catch` cleanup wc strm
  where
    pauseRequestBody inp th = Request $ inp { inpObjBody = readBody' }
      where
        readBody = inpObjBody inp
        readBody' = do
            T.pause th
            bs <- readBody
            T.resume th
            return bs

cleanup :: WorkerConf a -> a -> SomeException -> IO ()
cleanup WorkerConf{..} strm e
  -- killed by the local worker manager
  | Just ThreadKilled <- E.fromException e = E.throwIO ThreadKilled
  | otherwise                              = workerCleanup strm
