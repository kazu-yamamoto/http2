{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Server.Worker (
    worker,
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

makePushStream :: Context -> Stream -> IO (StreamId, Stream)
makePushStream ctx pstrm = do
    -- FLOW CONTROL: SETTINGS_MAX_CONCURRENT_STREAMS: send: respecting peer's limit
    (_, newstrm) <- openEvenStreamWait ctx
    let pid = streamNumber pstrm
    return (pid, newstrm)

----------------------------------------------------------------

pushStream
    :: Context
    -> Stream -- parent stream
    -> ValueTable -- request
    -> [PushPromise]
    -> IO (Maybe (IO ()))
pushStream _ _ _ [] = return Nothing
pushStream ctx@Context{..} pstrm reqvt pps0
    | len == 0 = return Nothing
    | otherwise = do
        pushable <- enablePush <$> readIORef peerSettings
        if pushable
            then do
                tvar <- newTVarIO 0
                lim <- push tvar pps0 0
                if lim == 0
                    then return Nothing
                    else return $ Just $ waiter lim tvar
            else return Nothing
  where
    len = length pps0
    increment tvar = atomically $ modifyTVar' tvar (+ 1)
    -- Checking if all push are done.
    waiter lim tvar = atomically $ do
        n <- readTVar tvar
        checkSTM (n >= lim)
    push _ [] n = return (n :: Int)
    push tvar (pp : pps) n = do
        forkManaged threadManager "H2 push" $ do
            timeoutKillThread threadManager $ \th -> do
                (pid, newstrm) <- makePushStream ctx pstrm
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
                syncWithSender ctx newstrm ot Nothing
                increment tvar
                processBody ctx th newstrm rsp
        push tvar pps (n + 1)

-- | This function is passed to workers.
--   They also pass 'Response's from a server to this function.
--   This function enqueues commands for the HTTP/2 sender.
response
    :: Context
    -> T.Handle
    -> Stream
    -> Request
    -> Response
    -> [PushPromise]
    -> IO ()
response ctx th strm (Request req) (Response rsp) pps = do
    mwait <- pushStream ctx strm reqvt pps
    case mwait of
        Nothing -> return ()
        Just wait -> wait -- all pushes are sent
    processBody ctx th strm rsp
  where
    (_, reqvt) = inpObjHeaders req

processBody :: Context -> T.Handle -> Stream -> OutObj -> IO ()
processBody ctx@Context{..} th strm rsp = do
    mtbq <- case outObjBody rsp of
        OutBodyStreaming strmbdy -> do
            tbq <- newTBQueueIO 10 -- fixme: hard coding: 10
            let push b = do
                    T.pause th
                    atomically $ writeTBQueue tbq (StreamingBuilder b Nothing)
                    T.resume th
                flush = atomically $ writeTBQueue tbq StreamingFlush
                finished =
                    atomically $
                        writeTBQueue tbq $
                            StreamingFinished (decCounter threadManager)
            forkManaged
                threadManager
                "H2 streaming"
                (strmbdy push flush `E.finally` finished)
            return $ Just tbq
        OutBodyStreamingUnmask _ ->
            error "response: server does not support OutBodyStreamingUnmask"
        _ -> return Nothing
    syncWithSender ctx strm (OObj rsp) mtbq

-- | Worker for server applications.
worker :: Server -> Context -> Stream -> InpObj -> IO ()
worker server ctx@Context{..} strm req =
    timeoutKillThread threadManager $ \th -> do
        -- FIXME: exception
        T.pause th
        let req' = pauseRequestBody th
        T.resume th
        T.tickle th
        let aux = Aux th mySockAddr peerSockAddr
            request = Request req'
        server request aux $ response ctx th strm request
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
