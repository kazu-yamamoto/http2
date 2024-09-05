{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Server.Worker (
    runWorker,
) where

import Control.Concurrent.STM
import Data.IORef
import Network.HTTP.Semantics
import Network.HTTP.Semantics.IO
import Network.HTTP.Semantics.Server
import Network.HTTP.Semantics.Server.Internal
import Network.HTTP.Types

import Imports hiding (insert)
import Network.HTTP2.Frame
import Network.HTTP2.H2

----------------------------------------------------------------

runWorker :: Config -> Server -> Launch
runWorker conf server ctx strm inpObj =
    forkManaged (threadManager ctx) label $
        worker conf server ctx strm inpObj
  where
    label = "H2 worker for stream " ++ show (streamNumber strm)

----------------------------------------------------------------

-- | Worker for server applications.
worker :: Config -> Server -> Context -> Stream -> InpObj -> IO ()
worker conf server ctx@Context{..} strm req =
    timeoutKillThread threadManager $ \postphone -> do
        -- FIXME: exception
        let req' = pauseRequestBody postphone
            aux = Aux postphone mySockAddr peerSockAddr
            request = Request req'
        server request aux $ sendResponse conf ctx postphone strm request
        adjustRxWindow ctx strm
  where
    pauseRequestBody postphone = req{inpObjBody = readBody'}
      where
        readBody = inpObjBody req
        readBody' = do
            bs <- readBody
            void $ postphone
            return bs

----------------------------------------------------------------

-- | This function is passed to workers.
--   They also pass 'Response's from a server to this function.
--   This function enqueues commands for the HTTP/2 sender.
sendResponse
    :: Config
    -> Context
    -> IO ()
    -> Stream
    -> Request
    -> Response
    -> [PushPromise]
    -> IO ()
sendResponse conf ctx postphone strm (Request req) (Response rsp) pps = do
    mwait <- pushStream conf ctx strm reqvt pps
    case mwait of
        Nothing -> return ()
        Just wait -> wait -- all pushes are sent
    sendHeaderBody conf ctx postphone strm rsp
  where
    (_, reqvt) = inpObjHeaders req

----------------------------------------------------------------

pushStream
    :: Config
    -> Context
    -> Stream -- parent stream
    -> ValueTable -- request
    -> [PushPromise]
    -> IO (Maybe (IO ()))
pushStream _ _ _ _ [] = return Nothing
pushStream conf ctx@Context{..} pstrm reqvt pps0
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
        check (n >= lim)
    push _ [] n = return (n :: Int)
    push tvar (pp : pps) n = do
        forkManaged threadManager "H2 server push" $ do
            timeoutKillThread threadManager $ \postphone -> do
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
                ((var, sync), out) <- prepareSync newstrm ot Nothing
                enqueueOutput outputQ out
                syncWithSender ctx newstrm var sync
                increment tvar
                sendHeaderBody conf ctx postphone newstrm rsp
        push tvar pps (n + 1)

----------------------------------------------------------------

makePushStream :: Context -> Stream -> IO (StreamId, Stream)
makePushStream ctx pstrm = do
    -- FLOW CONTROL: SETTINGS_MAX_CONCURRENT_STREAMS: send: respecting peer's limit
    (_, newstrm) <- openEvenStreamWait ctx
    let pid = streamNumber pstrm
    return (pid, newstrm)

----------------------------------------------------------------

sendHeaderBody :: Config -> Context -> IO () -> Stream -> OutObj -> IO ()
sendHeaderBody Config{..} ctx@Context{..} postphone strm OutObj{..} = do
    (mnext, mtbq) <- case outObjBody of
        OutBodyNone -> return (Nothing, Nothing)
        OutBodyFile (FileSpec path fileoff bytecount) -> do
            (pread, sentinel) <- confPositionReadMaker path
            let next = fillFileBodyGetNext pread fileoff bytecount sentinel
            return (Just next, Nothing)
        OutBodyBuilder builder -> do
            let next = fillBuilderBodyGetNext builder
            return (Just next, Nothing)
        OutBodyStreaming strmbdy -> do
            q <- sendStreaming ctx strm postphone $ \OutBodyIface{..} -> strmbdy outBodyPush outBodyFlush
            let next = nextForStreaming q
            return (Just next, Just q)
        OutBodyStreamingIface strmbdy -> do
            q <- sendStreaming ctx strm postphone strmbdy
            let next = nextForStreaming q
            return (Just next, Just q)
    ((var, sync), out) <-
        prepareSync strm (OHeader outObjHeaders mnext outObjTrailers) mtbq
    enqueueOutput outputQ out
    syncWithSender ctx strm var sync

----------------------------------------------------------------

sendStreaming
    :: Context
    -> Stream
    -> IO ()
    -> (OutBodyIface -> IO ())
    -> IO (TBQueue StreamingChunk)
sendStreaming Context{..} strm postphone strmbdy = do
    let label = "H2 streaming supporter for stream " ++ show (streamNumber strm)
    tbq <- newTBQueueIO 10 -- fixme: hard coding: 10
    forkManaged threadManager label $
        withOutBodyIface tbq id $ \iface -> do
            let iface' =
                    iface
                        { outBodyPush = \b -> do
                            outBodyPush iface b
                            postphone
                        , outBodyPushFinal = \b -> do
                            outBodyPushFinal iface b
                            postphone
                        }
            strmbdy iface'
    return tbq
