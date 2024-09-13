{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Server.Worker (
    runServer,
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

runServer :: Config -> Server -> Launch
runServer conf@Config{..} server ctx@Context{..} strm req =
    forkManaged threadManager label $
        withTimeout threadManager confTimeout $ \(tovar, delayTimeout) -> do
            -- FIXME: exception
            let req' = pauseRequestBody delayTimeout
                aux = Aux delayTimeout mySockAddr peerSockAddr
                request = Request req'
                lc =
                    LoopCheck
                        { lcTBQ = Nothing
                        , lcTimeout = tovar
                        , lcWindow = streamTxFlow strm
                        }
            server request aux $ sendResponse conf ctx lc delayTimeout strm request
            adjustRxWindow ctx strm
  where
    label = "H2 response sender for stream " ++ show (streamNumber strm)
    pauseRequestBody delayTimeout = req{inpObjBody = readBody'}
      where
        readBody = inpObjBody req
        readBody' = do
            bs <- readBody
            void delayTimeout
            return bs

----------------------------------------------------------------

-- | This function is passed to workers.
--   They also pass 'Response's from a server to this function.
--   This function enqueues commands for the HTTP/2 sender.
sendResponse
    :: Config
    -> Context
    -> LoopCheck
    -> IO ()
    -> Stream
    -> Request
    -> Response
    -> [PushPromise]
    -> IO ()
sendResponse conf ctx lc delayTimeout strm (Request req) (Response rsp) pps =
    forkManaged (threadManager ctx) label $ do
        mwait <- pushStream conf ctx strm reqvt pps
        case mwait of
            Nothing -> return ()
            Just wait -> wait -- all pushes are sent
        sendHeaderBody conf ctx lc delayTimeout strm rsp
  where
    (_, reqvt) = inpObjHeaders req
    label = "H2 worker for stream " ++ show (streamNumber strm)

----------------------------------------------------------------

pushStream
    :: Config
    -> Context
    -> Stream -- parent stream
    -> ValueTable -- request
    -> [PushPromise]
    -> IO (Maybe (IO ()))
pushStream _ _ _ _ [] = return Nothing
pushStream conf@Config{..} ctx@Context{..} pstrm reqvt pps0
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
            withTimeout threadManager confTimeout $ \(tovar, delayTimeout) -> do
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
                increment tvar
                let lc =
                        LoopCheck
                            { lcTBQ = Nothing
                            , lcTimeout = tovar
                            , lcWindow = streamTxFlow newstrm
                            }
                syncWithSender ctx newstrm ot lc
                sendHeaderBody conf ctx lc delayTimeout newstrm rsp
        push tvar pps (n + 1)

----------------------------------------------------------------

makePushStream :: Context -> Stream -> IO (StreamId, Stream)
makePushStream ctx pstrm = do
    -- FLOW CONTROL: SETTINGS_MAX_CONCURRENT_STREAMS: send: respecting peer's limit
    (_, newstrm) <- openEvenStreamWait ctx
    let pid = streamNumber pstrm
    return (pid, newstrm)

----------------------------------------------------------------

sendHeaderBody
    :: Config
    -> Context
    -> LoopCheck
    -> IO ()
    -> Stream
    -> OutObj
    -> IO ()
sendHeaderBody Config{..} ctx lc delayTimeout strm OutObj{..} = do
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
            q <- sendStreaming ctx strm delayTimeout $ \OutBodyIface{..} -> strmbdy outBodyPush outBodyFlush
            let next = nextForStreaming q
            return (Just next, Just q)
        OutBodyStreamingIface strmbdy -> do
            q <- sendStreaming ctx strm delayTimeout strmbdy
            let next = nextForStreaming q
            return (Just next, Just q)
    let lc' = lc{lcTBQ = mtbq}
    syncWithSender ctx strm (OHeader outObjHeaders mnext outObjTrailers) lc'

----------------------------------------------------------------

sendStreaming
    :: Context
    -> Stream
    -> IO ()
    -> (OutBodyIface -> IO ())
    -> IO (TBQueue StreamingChunk)
sendStreaming Context{..} strm delayTimeout strmbdy = do
    tbq <- newTBQueueIO 10 -- fixme: hard coding: 10
    forkManaged threadManager label $
        withOutBodyIface tbq id $ \iface -> do
            let iface' =
                    iface
                        { outBodyPush = \b -> do
                            outBodyPush iface b
                            delayTimeout
                        , outBodyPushFinal = \b -> do
                            outBodyPushFinal iface b
                            delayTimeout
                        }
            strmbdy iface'
    return tbq
  where
    label = "H2 response streaming sender for " ++ show (streamNumber strm)
