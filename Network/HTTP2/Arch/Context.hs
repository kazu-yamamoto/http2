{-# LANGUAGE NamedFieldPuns #-}

module Network.HTTP2.Arch.Context where

import Data.IORef
import Network.HTTP.Types (Method)
import Network.Socket (SockAddr)
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
  -- Settings
  , myFirstSettings    :: IORef Bool
  , myPendingAlist     :: IORef (Maybe SettingsList)
  , mySettings         :: IORef Settings
  , peerSettings       :: IORef Settings
  , streamTable        :: StreamTable
  , concurrency        :: IORef Int
  -- | RFC 9113 says "Other frames (from any stream) MUST NOT
  --   occur between the HEADERS frame and any CONTINUATION
  --   frames that might follow". This field is used to implement
  --   this requirement.
  , continued          :: IORef (Maybe StreamId)
  , myStreamId         :: IORef StreamId
  , peerStreamId       :: IORef StreamId
  , outputBufferLimit  :: IORef Int
  , outputQ            :: TQueue (Output Stream)
  , outputQStreamID    :: TVar StreamId
  , controlQ           :: TQueue Control
  , encodeDynamicTable :: DynamicTable
  , decodeDynamicTable :: DynamicTable
  -- the connection window for sending data
  , txConnectionWindow :: TVar WindowSize
  -- window update for receiving data
  , rxConnectionInc    :: IORef WindowSize -- this is diff
  , pingRate           :: Rate
  , settingsRate       :: Rate
  , emptyFrameRate     :: Rate
  , mySockAddr         :: Maybe SockAddr
  , peerSockAddr       :: Maybe SockAddr
  }

----------------------------------------------------------------

newContext :: RoleInfo -> BufferSize -> Maybe SockAddr -> Maybe SockAddr -> IO Context
newContext rinfo siz mysa peersa =
    Context rl rinfo
               <$> newIORef False
               <*> newIORef Nothing
               <*> newIORef defaultSettings
               <*> newIORef defaultSettings
               <*> newStreamTable
               <*> newIORef 0
               <*> newIORef Nothing
               <*> newIORef sid0
               <*> newIORef 0
               <*> newIORef buflim
               <*> newTQueueIO
               <*> newTVarIO sid0
               <*> newTQueueIO
               -- My SETTINGS_HEADER_TABLE_SIZE
               <*> newDynamicTableForEncoding defaultDynamicTableSize
               <*> newDynamicTableForDecoding defaultDynamicTableSize 4096
               <*> newTVarIO defaultWindowSize
               <*> newIORef 0
               <*> newRate
               <*> newRate
               <*> newRate
               <*> return mysa
               <*> return peersa
   where
     rl = case rinfo of
       RIC{} -> Client
       _     -> Server
     sid0 | rl == Client = 1
          | otherwise    = 2
     dlim = defaultPayloadLength + frameHeaderLength
     buflim | siz >= dlim = dlim
            | otherwise   = siz

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
openStream ctx@Context{streamTable, peerSettings} sid ftyp = do
    ws <- initialWindowSize <$> readIORef peerSettings
    newstrm <- newStream sid ws
    when (ftyp == FrameHeaders || ftyp == FramePushPromise) $ opened ctx newstrm
    insert streamTable sid newstrm
    return newstrm
