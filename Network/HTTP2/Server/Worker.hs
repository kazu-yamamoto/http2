{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Server.Worker (
    worker,
    WorkerConf (..),
    fromContext,
) where

import Data.IORef
import Network.HTTP.Semantics
import Network.HTTP.Semantics.IO
import Network.HTTP.Semantics.Server
import Network.HTTP.Semantics.Server.Internal
import Network.HTTP.Types
import qualified System.TimeManager as T
import qualified UnliftIO.Exception as E
import UnliftIO.STM

import Imports hiding (insert)
import Network.HTTP2.Frame
import Network.HTTP2.H2

----------------------------------------------------------------

data WorkerConf a = WorkerConf
    { writeOutputQ :: Output a -> IO ()
    , isPushable :: IO Bool
    , makePushStream :: a -> PushPromise -> IO (StreamId, a)
    }

fromContext :: Context -> WorkerConf Stream
fromContext ctx@Context{..} =
    WorkerConf
        { writeOutputQ = enqueueOutput outputQ
        , -- Peer SETTINGS_ENABLE_PUSH
          isPushable = enablePush <$> readIORef peerSettings
        , -- Peer SETTINGS_INITIAL_WINDOW_SIZE
          makePushStream = \pstrm _ -> do
            -- FLOW CONTROL: SETTINGS_MAX_CONCURRENT_STREAMS: send: respecting peer's limit
            (_, newstrm) <- openEvenStreamWait ctx
            let pid = streamNumber pstrm
            return (pid, newstrm)
        }

----------------------------------------------------------------

pushStream
    :: WorkerConf a
    -> a -- parent stream
    -> ValueTable -- request
    -> [PushPromise]
    -> IO OutputType
pushStream _ _ _ [] = return OObj
pushStream WorkerConf{..} pstrm reqvt pps0
    | len == 0 = return OObj
    | otherwise = do
        pushable <- isPushable
        if pushable
            then do
                tvar <- newTVarIO 0
                lim <- push tvar pps0 0
                if lim == 0
                    then return OObj
                    else return $ OWait (waiter lim tvar)
            else return OObj
  where
    len = length pps0
    increment tvar = atomically $ modifyTVar' tvar (+ 1)
    waiter lim tvar = atomically $ do
        n <- readTVar tvar
        checkSTM (n >= lim)
    push _ [] n = return (n :: Int)
    push tvar (pp : pps) n = do
        (pid, newstrm) <- makePushStream pstrm pp
        let scheme = fromJust $ getFieldValue tokenScheme reqvt
            -- fixme: this value can be Nothing
            auth =
                fromJust
                    ( getFieldValue tokenAuthority reqvt
                        <|> getFieldValue tokenHost reqvt
                    )
            path = promiseRequestPath pp
            promiseRequest =
                [ (tokenMethod, methodGet)
                , (tokenScheme, scheme)
                , (tokenAuthority, auth)
                , (tokenPath, path)
                ]
            ot = OPush promiseRequest pid
            Response rsp = promiseResponse pp
            out = Output newstrm rsp ot Nothing $ increment tvar
        writeOutputQ out
        push tvar pps (n + 1)

-- | This function is passed to workers.
--   They also pass 'Response's from a server to this function.
--   This function enqueues commands for the HTTP/2 sender.
response
    :: WorkerConf a
    -> Manager
    -> T.Handle
    -> a
    -> Request
    -> Response
    -> [PushPromise]
    -> IO ()
response wc@WorkerConf{..} mgr th strm (Request req) (Response rsp) pps = case outObjBody rsp of
    OutBodyNone ->
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
        let push b = do
                T.pause th
                atomically $ writeTBQueue tbq (StreamingBuilder b Nothing)
                T.resume th
            flush = atomically $ writeTBQueue tbq StreamingFlush
            finished = atomically $ writeTBQueue tbq $ StreamingFinished (decCounter mgr)
        strmbdy push flush `E.finally` finished
    OutBodyStreamingUnmask _ ->
        error "response: server does not support OutBodyStreamingUnmask"
  where
    (_, reqvt) = inpObjHeaders req

-- | Worker for server applications.
worker :: WorkerConf Stream -> Server -> Context -> Stream -> InpObj -> IO ()
worker wc server ctx@Context{..} strm req =
    timeoutKillThread threadManager $ \th -> do
        -- FIXME: exception
        T.pause th
        let req' = pauseRequestBody th
        T.resume th
        T.tickle th
        let aux = Aux th mySockAddr peerSockAddr
            request = Request req'
        server request aux $
            response wc threadManager th strm request
        adjustRxWindow ctx strm
  where
    pauseRequestBody th = req{inpObjBody = readBody'}
      where
        readBody = inpObjBody req
        readBody' = do
            T.pause th
            bs <- readBody
            T.resume th
            return bs
