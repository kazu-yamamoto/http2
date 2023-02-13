{-# LANGUAGE NamedFieldPuns #-}

module Network.HTTP2.Arch.Context where

import Data.IORef
import Network.HTTP.Types (Method)
import UnliftIO.STM

import Imports hiding (insert)
import Network.HPACK
import Network.HTTP2.Arch.Cache (Cache, emptyCache)
import qualified Network.HTTP2.Arch.Cache as Cache
import Network.HTTP2.Arch.Rate
import Network.HTTP2.Arch.Stream
import Network.HTTP2.Arch.Types
import Network.HTTP2.Frame

data Role = Client | Server deriving (Eq,Show)

----------------------------------------------------------------

data RoleInfo = RIS ServerInfo | RIC ClientInfo

data ServerInfo = ServerInfo {
    inputQ :: TQueue (Input Stream)
  }

data ClientInfo = ClientInfo {
    scheme    :: ByteString
  , authority :: ByteString
  , cache     :: IORef (Cache (Method,ByteString) Stream)
  }

toServerInfo :: RoleInfo -> ServerInfo
toServerInfo (RIS x) = x
toServerInfo _       = error "toServerInfo"

toClientInfo :: RoleInfo -> ClientInfo
toClientInfo (RIC x) = x
toClientInfo _       = error "toClientInfo"

newServerInfo :: IO RoleInfo
newServerInfo = RIS . ServerInfo <$> newTQueueIO

newClientInfo :: ByteString -> ByteString -> Int -> IO RoleInfo
newClientInfo scm auth lim =  RIC . ClientInfo scm auth <$> newIORef (emptyCache lim)

insertCache :: Method -> ByteString -> Stream -> RoleInfo -> IO ()
insertCache m path v (RIC (ClientInfo _ _ ref)) = atomicModifyIORef' ref $ \c ->
  (Cache.insert (m,path) v c, ())
insertCache _ _ _ _ = error "insertCache"

lookupCache :: Method -> ByteString -> RoleInfo -> IO (Maybe Stream)
lookupCache m path (RIC (ClientInfo _ _ ref)) = Cache.lookup (m,path) <$> readIORef ref
lookupCache _ _ _ = error "lookupCache"

----------------------------------------------------------------

-- | The context for HTTP/2 connection.
data Context = Context {
    role               :: Role
  , roleInfo           :: RoleInfo
  -- HTTP/2 settings received from a browser
  , http2settings      :: IORef Settings
  , firstSettings      :: IORef Bool
  , streamTable        :: StreamTable
  , concurrency        :: IORef Int
  -- | RFC 9113 says "Other frames (from any stream) MUST NOT
  --   occur between the HEADERS frame and any CONTINUATION
  --   frames that might follow". This field is used to implement
  --   this requirement.
  , continued          :: IORef (Maybe StreamId)
  , myStreamId         :: IORef StreamId
  , peerStreamId       :: IORef StreamId
  , outputQ            :: TQueue (Output Stream)
  , outputQStreamID    :: TVar StreamId
  , controlQ           :: TQueue Control
  , encodeDynamicTable :: DynamicTable
  , decodeDynamicTable :: DynamicTable
  -- the connection window for data from a server to a browser.
  , connectionWindow   :: TVar WindowSize
  , pingRate           :: Rate
  , settingsRate       :: Rate
  , emptyFrameRate     :: Rate
  }

----------------------------------------------------------------

newContext :: RoleInfo -> IO Context
newContext rinfo =
    Context rl rinfo
               <$> newIORef defaultSettings
               <*> newIORef False
               <*> newStreamTable
               <*> newIORef 0
               <*> newIORef Nothing
               <*> newIORef sid0
               <*> newIORef 0
               <*> newTQueueIO
               <*> newTVarIO sid0
               <*> newTQueueIO
               <*> newDynamicTableForEncoding defaultDynamicTableSize
               <*> newDynamicTableForDecoding defaultDynamicTableSize 4096
               <*> newTVarIO defaultInitialWindowSize
               <*> newRate
               <*> newRate
               <*> newRate
   where
     rl = case rinfo of
       RIC{} -> Client
       _     -> Server
     sid0 | rl == Client = 1
          | otherwise    = 2

----------------------------------------------------------------

isClient :: Context -> Bool
isClient ctx = role ctx == Client

isServer :: Context -> Bool
isServer ctx = role ctx == Server

----------------------------------------------------------------

getMyNewStreamId :: Context -> IO StreamId
getMyNewStreamId ctx = atomicModifyIORef' (myStreamId ctx) inc2
  where
    inc2 n = let n' = n + 2 in (n', n)

getPeerStreamID :: Context -> IO StreamId
getPeerStreamID ctx = readIORef $ peerStreamId ctx

setPeerStreamID :: Context -> StreamId -> IO ()
setPeerStreamID ctx sid =  writeIORef (peerStreamId ctx) sid

----------------------------------------------------------------

{-# INLINE setStreamState #-}
setStreamState :: Context -> Stream -> StreamState -> IO ()
setStreamState _ Stream{streamState} val = writeIORef streamState val

opened :: Context -> Stream -> IO ()
opened ctx@Context{concurrency} strm = do
    atomicModifyIORef' concurrency (\x -> (x+1,()))
    setStreamState ctx strm (Open JustOpened)

halfClosedRemote :: Context -> Stream -> IO ()
halfClosedRemote ctx stream@Stream{streamState} = do
    closingCode <- atomicModifyIORef streamState closeHalf
    traverse_ (closed ctx stream) closingCode
  where
    closeHalf :: StreamState -> (StreamState, Maybe ClosedCode)
    closeHalf x@(Closed _)         = (x, Nothing)
    closeHalf (HalfClosedLocal cc) = (Closed cc, Just cc)
    closeHalf _                    = (HalfClosedRemote, Nothing)

halfClosedLocal :: Context -> Stream -> ClosedCode -> IO ()
halfClosedLocal ctx stream@Stream{streamState} cc = do
    shouldFinalize <- atomicModifyIORef streamState closeHalf
    when shouldFinalize $
        closed ctx stream cc
  where
    closeHalf :: StreamState -> (StreamState, Bool)
    closeHalf x@(Closed _)     = (x, False)
    closeHalf HalfClosedRemote = (Closed cc, True)
    closeHalf _                = (HalfClosedLocal cc, False)

closed :: Context -> Stream -> ClosedCode -> IO ()
closed ctx@Context{concurrency,streamTable} strm@Stream{streamNumber} cc = do
    remove streamTable streamNumber
    -- TODO: prevent double-counting
    atomicModifyIORef' concurrency (\x -> (x-1,()))
    setStreamState ctx strm (Closed cc) -- anyway

openStream :: Context -> StreamId -> FrameType -> IO Stream
openStream ctx@Context{streamTable, http2settings} sid ftyp = do
    ws <- initialWindowSize <$> readIORef http2settings
    newstrm <- newStream sid $ fromIntegral ws
    when (ftyp == FrameHeaders || ftyp == FramePushPromise) $ opened ctx newstrm
    insert streamTable sid newstrm
    return newstrm
