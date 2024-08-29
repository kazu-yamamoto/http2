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
        checkSTM (n >= lim)
    push _ [] n = return (n :: Int)
    push tvar (pp : pps) n = do
        forkManaged threadManager "H2 server push" $ do
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
                ((var, sync), out) <- prepareSync newstrm ot Nothing
                enqueueOutput outputQ out
                syncWithSender ctx newstrm var sync
                increment tvar
                sendHeaderBody conf ctx th newstrm rsp
        push tvar pps (n + 1)

-- | This function is passed to workers.
--   They also pass 'Response's from a server to this function.
--   This function enqueues commands for the HTTP/2 sender.
sendResponse
    :: Config
    -> Context
    -> T.Handle
    -> Stream
    -> Request
    -> Response
    -> [PushPromise]
    -> IO ()
sendResponse conf ctx th strm (Request req) (Response rsp) pps = do
    mwait <- pushStream conf ctx strm reqvt pps
    case mwait of
        Nothing -> return ()
        Just wait -> wait -- all pushes are sent
    sendHeaderBody conf ctx th strm rsp
  where
    (_, reqvt) = inpObjHeaders req

sendHeaderBody :: Config -> Context -> T.Handle -> Stream -> OutObj -> IO ()
sendHeaderBody Config{..} ctx@Context{..} th strm OutObj{..} = do
    (mnext, mtbq) <- case outObjBody of
        OutBodyNone -> return (Nothing, Nothing)
        OutBodyFile (FileSpec path fileoff bytecount) -> do
            (pread, closerOrRefresher) <- confPositionReadMaker path
            refresh <- case closerOrRefresher of
                Closer closer -> timeoutClose threadManager closer
                Refresher refresher -> return refresher
            let next = fillFileBodyGetNext pread fileoff bytecount refresh
            return (Just next, Nothing)
        OutBodyBuilder builder -> do
            let next = fillBuilderBodyGetNext builder
            return (Just next, Nothing)
        OutBodyStreaming strmbdy -> do
            q <- sendStreaming ctx strm th $ \OutBodyIface{..} -> strmbdy outBodyPush outBodyFlush
            let next = nextForStreaming q
            return (Just next, Just q)
        OutBodyStreamingIface strmbdy -> do
            q <- sendStreaming ctx strm th strmbdy
            let next = nextForStreaming q
            return (Just next, Just q)
    ((var, sync), out) <-
        prepareSync strm (OHeader outObjHeaders mnext outObjTrailers) mtbq
    enqueueOutput outputQ out
    syncWithSender ctx strm var sync
  where
    nextForStreaming
        :: TBQueue StreamingChunk
        -> DynaNext
    nextForStreaming tbq =
        let takeQ = atomically $ tryReadTBQueue tbq
            next = fillStreamBodyGetNext takeQ
         in next

sendStreaming
    :: Context
    -> Stream
    -> T.Handle
    -> (OutBodyIface -> IO ())
    -> IO (TBQueue StreamingChunk)
sendStreaming Context{..} strm th strmbdy = do
    tbq <- newTBQueueIO 10 -- fixme: hard coding: 10
    finishedOrCancelled <- newTVarIO False
    let label = "H2 streaming supporter for stream " ++ show (streamNumber strm)
    forkManaged threadManager label $ do
        let iface =
                OutBodyIface
                    { outBodyUnmask = id
                    , outBodyPush = \b -> do
                        T.pause th
                        atomically $ writeTBQueue tbq (StreamingBuilder b NotEndOfStream)
                        T.resume th
                    , outBodyPushFinal = \b -> do
                        T.pause th
                        atomically $ do
                          writeTVar finishedOrCancelled True
                          writeTBQueue tbq $ StreamingBuilder b (EndOfStream Nothing)
                          writeTBQueue tbq $ StreamingFinished Nothing
                        T.resume th
                    , outBodyFlush = atomically $ writeTBQueue tbq StreamingFlush
                    , outBodyCancel = \mErr -> atomically $ do
                        already <- readTVar finishedOrCancelled
                        writeTVar finishedOrCancelled True
                        unless already $
                          writeTBQueue tbq (StreamingCancelled mErr)
                    }
            finished = atomically $ do
              already <- readTVar finishedOrCancelled
              writeTVar finishedOrCancelled True
              unless already $
                writeTBQueue tbq $ StreamingFinished Nothing
        strmbdy iface `E.finally` finished
    return tbq

-- | Worker for server applications.
worker :: Config -> Server -> Context -> Stream -> InpObj -> IO ()
worker conf server ctx@Context{..} strm req =
    timeoutKillThread threadManager $ \th -> do
        -- FIXME: exception
        T.pause th
        let req' = pauseRequestBody th
        T.resume th
        T.tickle th
        let aux = Aux th mySockAddr peerSockAddr
            request = Request req'
        server request aux $ sendResponse conf ctx th strm request
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
