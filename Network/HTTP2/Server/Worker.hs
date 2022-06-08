{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Server.Worker (
    worker
  , WorkerConf(..)
  , fromContext
  ) where

import Control.Concurrent.STM
import Control.Exception (SomeException(..), AsyncException(..))
import qualified Control.Exception as E
import Data.IORef
import qualified Network.HTTP.Types as H
import qualified System.TimeManager as T

import Imports hiding (insert)
import Network.HPACK
import Network.HPACK.Token
import Network.HTTP2.Arch
import Network.HTTP2.Frame
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
    readInputQ = atomically $ readTQueue $ inputQ $ toServerInfo roleInfo
  , writeOutputQ = enqueueOutput outputQ
  , workerCleanup = \strm -> do
        closed ctx strm Killed
        let frame = resetFrame InternalError (streamNumber strm)
        enqueueControl controlQ $ CFrame frame
  , isPushable = enablePush <$> readIORef http2settings
  , insertStream = insert streamTable
  , makePushStream = \pstrm _ -> do
        ws <- initialWindowSize <$> readIORef http2settings
        sid <- getMyNewStreamId ctx
        newstrm <- newPushStream sid ws
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
response :: WorkerConf a -> Manager -> T.Handle -> ThreadContinue -> a -> Request -> Response -> [PushPromise] -> IO ()
response wc@WorkerConf{..} mgr th tconf strm (Request req) (Response rsp) pps = case outObjBody rsp of
  OutBodyNone -> do
      setThreadContinue tconf True
      writeOutputQ $ Output strm rsp OObj Nothing (return ())
  OutBodyBuilder _ -> do
      otyp <- pushStream wc strm reqvt pps
      setThreadContinue tconf True
      writeOutputQ $ Output strm rsp otyp Nothing (return ())
  OutBodyFile _ -> do
      otyp <- pushStream wc strm reqvt pps
      setThreadContinue tconf True
      writeOutputQ $ Output strm rsp otyp Nothing (return ())
  OutBodyStreaming strmbdy -> do
      otyp <- pushStream wc strm reqvt pps
      -- We must not exit this server application.
      -- If the application exits, streaming would be also closed.
      -- So, this work occupies this thread.
      --
      -- We need to increase the number of workers.
      spawnAction mgr
      -- After this work, this thread stops to decease
      -- the number of workers.
      setThreadContinue tconf False
      -- Since streaming body is loop, we cannot control it.
      -- So, let's serialize 'Builder' with a designated queue.
      tbq <- newTBQueueIO 10 -- fixme: hard coding: 10
      writeOutputQ $ Output strm rsp otyp (Just tbq) (return ())
      let push b = do
            T.pause th
            atomically $ writeTBQueue tbq (StreamingBuilder b)
            T.resume th
          flush  = atomically $ writeTBQueue tbq StreamingFlush
      strmbdy push flush
      atomically $ writeTBQueue tbq StreamingFinished
      deleteMyId mgr
  where
    (_,reqvt) = inpObjHeaders req

-- | Worker for server applications.
worker :: WorkerConf a -> Manager -> Server -> Action
worker wc@WorkerConf{..} mgr server = do
    sinfo <- newStreamInfo
    tcont <- newThreadContinue
    timeoutKillThread mgr $ go sinfo tcont
  where
    go sinfo tcont th = do
        setThreadContinue tcont True
        ex <- E.try $ do
            T.pause th
            Input strm req <- readInputQ
            let req' = pauseRequestBody req th
            setStreamInfo sinfo strm
            T.resume th
            T.tickle th
            let aux = Aux th
            server (Request req') aux $ response wc mgr th tcont strm (Request req')
        cont1 <- case ex of
            Right () -> return True
            Left e@(SomeException _)
              -- killed by the local worker manager
              | Just ThreadKilled    <- E.fromException e -> return False
              -- killed by the local timeout manager
              | Just T.TimeoutThread <- E.fromException e -> do
                  cleanup sinfo
                  return True
              | otherwise -> do
                  cleanup sinfo
                  return True
        cont2 <- getThreadContinue tcont
        clearStreamInfo sinfo
        when (cont1 && cont2) $ go sinfo tcont th
    pauseRequestBody req th = req { inpObjBody = readBody' }
      where
        readBody = inpObjBody req
        readBody' = do
            T.pause th
            bs <- readBody
            T.resume th
            return bs
    cleanup sinfo = do
        minp <- getStreamInfo sinfo
        case minp of
            Nothing   -> return ()
            Just strm -> workerCleanup strm

----------------------------------------------------------------

--   A reference is shared by a responder and its worker.
--   The reference refers a value of this type as a return value.
--   If 'True', the worker continue to serve requests.
--   Otherwise, the worker get finished.
newtype ThreadContinue = ThreadContinue (IORef Bool)

{-# INLINE newThreadContinue #-}
newThreadContinue :: IO ThreadContinue
newThreadContinue = ThreadContinue <$> newIORef True

{-# INLINE setThreadContinue #-}
setThreadContinue :: ThreadContinue -> Bool -> IO ()
setThreadContinue (ThreadContinue ref) x = writeIORef ref x

{-# INLINE getThreadContinue #-}
getThreadContinue :: ThreadContinue -> IO Bool
getThreadContinue (ThreadContinue ref) = readIORef ref

----------------------------------------------------------------

-- | The type for cleaning up.
newtype StreamInfo a = StreamInfo (IORef (Maybe a))

{-# INLINE newStreamInfo #-}
newStreamInfo :: IO (StreamInfo a)
newStreamInfo = StreamInfo <$> newIORef Nothing

{-# INLINE clearStreamInfo #-}
clearStreamInfo :: StreamInfo a -> IO ()
clearStreamInfo (StreamInfo ref) = writeIORef ref Nothing

{-# INLINE setStreamInfo #-}
setStreamInfo :: StreamInfo a -> a -> IO ()
setStreamInfo (StreamInfo ref) inp = writeIORef ref $ Just inp

{-# INLINE getStreamInfo #-}
getStreamInfo :: StreamInfo a -> IO (Maybe a)
getStreamInfo (StreamInfo ref) = readIORef ref
