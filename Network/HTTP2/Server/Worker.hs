{-# LANGUAGE OverloadedStrings #-}
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
import qualified System.ThreadManager as T

import Imports hiding (insert)
import Network.HTTP2.Frame
import Network.HTTP2.H2

----------------------------------------------------------------

runServer :: Config -> Server -> Launch
runServer conf server ctx@Context{..} strm req =
    T.forkManagedTimeout threadManager label $ \th -> do
        let req' = pauseRequestBody th
            aux = Aux th mySockAddr peerSockAddr
            request = Request req'
        lc <- newLoopCheck strm Nothing
        server request aux $ sendResponse conf ctx lc strm request
        adjustRxWindow ctx strm
        modifyPeerLastStreamId ctx $ streamNumber strm
  where
    label = "H2 response sender for stream " ++ show (streamNumber strm)
    pauseRequestBody th = req{inpObjBody = readBody'}
      where
        readBody = inpObjBody req
        readBody' = do
            T.pause th
            bs <- readBody
            T.resume th -- this is the same as 'tickle'
            return bs

----------------------------------------------------------------

-- | This function is passed to workers.
--   They also pass 'Response's from a server to this function.
--   This function enqueues commands for the HTTP/2 sender.
sendResponse
    :: Config
    -> Context
    -> LoopCheck
    -> Stream
    -> Request
    -> Response
    -> [PushPromise]
    -> IO ()
sendResponse conf ctx lc strm (Request req) (Response rsp) pps = do
    mwait <- pushStream conf ctx strm reqvt pps
    case mwait of
        Nothing -> return ()
        Just wait -> wait -- all pushes are sent
    sendHeaderBody conf ctx lc strm rsp
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
        T.forkManaged threadManager "H2 server push" $ do
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
            lc <- newLoopCheck newstrm Nothing
            syncWithSender ctx newstrm ot lc
            sendHeaderBody conf ctx lc newstrm rsp
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
    -> Stream
    -> OutObj
    -> IO ()
sendHeaderBody Config{..} ctx lc strm OutObj{..} = do
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
            q <- sendStreaming ctx strm $ \OutBodyIface{..} -> strmbdy outBodyPush outBodyFlush
            let next = nextForStreaming q
            return (Just next, Just q)
        OutBodyStreamingIface strmbdy -> do
            q <- sendStreaming ctx strm strmbdy
            let next = nextForStreaming q
            return (Just next, Just q)
    let lc' = lc{lcTBQ = mtbq}
    syncWithSender ctx strm (OHeader outObjHeaders mnext outObjTrailers) lc'

----------------------------------------------------------------

sendStreaming
    :: Context
    -> Stream
    -> (OutBodyIface -> IO ())
    -> IO (TBQueue StreamingChunk)
sendStreaming Context{..} strm strmbdy = do
    tbq <- newTBQueueIO 10 -- fixme: hard coding: 10
    T.forkManagedTimeout threadManager label $ \th ->
        withOutBodyIface tbq id $ \iface -> do
            let iface' =
                    iface
                        { outBodyPush = \b -> do
                            T.pause th
                            outBodyPush iface b
                            T.resume th -- this is the same as 'tickle'
                        , outBodyPushFinal = \b -> do
                            T.pause th
                            outBodyPushFinal iface b
                            T.resume th -- this is the same as 'tickle'
                        }
            strmbdy iface'
    return tbq
  where
    label = "H2 response streaming sender for " ++ show (streamNumber strm)
