{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Context where

import Control.Exception
import Data.IORef
import Network.Control
import Network.HTTP.Types (Method)
import Network.Socket (SockAddr)
import qualified UnliftIO.Exception as E
import UnliftIO.STM

import Imports hiding (insert)
import Network.HPACK
import Network.HTTP2.Frame
import Network.HTTP2.H2.Stream
import Network.HTTP2.H2.StreamTable
import Network.HTTP2.H2.Types

data Role = Client | Server deriving (Eq, Show)

----------------------------------------------------------------

data RoleInfo = RIS ServerInfo | RIC ClientInfo

data ServerInfo = ServerInfo
    { inputQ :: TQueue (Input Stream)
    }

data ClientInfo = ClientInfo
    { scheme :: ByteString
    , authority :: ByteString
    }

toServerInfo :: RoleInfo -> ServerInfo
toServerInfo (RIS x) = x
toServerInfo _ = error "toServerInfo"

toClientInfo :: RoleInfo -> ClientInfo
toClientInfo (RIC x) = x
toClientInfo _ = error "toClientInfo"

newServerInfo :: IO RoleInfo
newServerInfo = RIS . ServerInfo <$> newTQueueIO

newClientInfo :: ByteString -> ByteString -> RoleInfo
newClientInfo scm auth = RIC $ ClientInfo scm auth

----------------------------------------------------------------

-- | The context for HTTP/2 connection.
data Context = Context
    { role :: Role
    , roleInfo :: RoleInfo
    , -- Settings
      mySettingAlist :: SettingsList -- to be myPendingAlist
    , myFirstSettings :: IORef Bool
    , myPendingAlist :: IORef (Maybe SettingsList) -- to be mySettings
    , mySettings :: IORef Settings
    , peerSettings :: IORef Settings
    , oddStreamTable :: TVar OddStreamTable
    , evenStreamTable :: TVar EvenStreamTable
    , continued :: IORef (Maybe StreamId)
    -- ^ RFC 9113 says "Other frames (from any stream) MUST NOT
    --   occur between the HEADERS frame and any CONTINUATION
    --   frames that might follow". This field is used to implement
    --   this requirement.
    , myStreamId :: TVar StreamId
    , peerStreamId :: IORef StreamId
    , outputBufferLimit :: IORef Int
    , outputQ :: TQueue (Output Stream)
    , outputQStreamID :: TVar StreamId
    , controlQ :: TQueue Control
    , encodeDynamicTable :: DynamicTable
    , decodeDynamicTable :: DynamicTable
    , -- the connection window for sending data
      txConnectionWindow :: TVar WindowSize
    , rxFlow :: IORef RxFlow
    , pingRate :: Rate
    , settingsRate :: Rate
    , emptyFrameRate :: Rate
    , rstRate :: Rate
    , mySockAddr :: SockAddr
    , peerSockAddr :: SockAddr
    }

----------------------------------------------------------------

newContext
    :: RoleInfo
    -> Int
    -> BufferSize
    -> SockAddr
    -> SockAddr
    -> SettingsList
    -> IO Context
newContext rinfo cacheSiz siz mysa peersa settingAlist =
    Context rl rinfo settingAlist
        <$> newIORef False
        <*> newIORef Nothing
        <*> newIORef defaultSettings
        <*> newIORef defaultSettings
        <*> newTVarIO emptyOddStreamTable
        <*> newTVarIO (emptyEvenStreamTable cacheSiz)
        <*> newIORef Nothing
        <*> newTVarIO sid0
        <*> newIORef 0
        <*> newIORef buflim
        <*> newTQueueIO
        <*> newTVarIO sid0
        <*> newTQueueIO
        -- My SETTINGS_HEADER_TABLE_SIZE
        <*> newDynamicTableForEncoding defaultDynamicTableSize
        <*> newDynamicTableForDecoding defaultDynamicTableSize 4096
        <*> newTVarIO defaultWindowSize
        <*> newIORef (newRxFlow (64 * 1024))
        <*> newRate
        <*> newRate
        <*> newRate
        <*> newRate
        <*> return mysa
        <*> return peersa
  where
    rl = case rinfo of
        RIC{} -> Client
        _ -> Server
    sid0
        | rl == Client = 1
        | otherwise = 2
    dlim = defaultPayloadLength + frameHeaderLength
    buflim
        | siz >= dlim = dlim
        | otherwise = siz

----------------------------------------------------------------

isClient :: Context -> Bool
isClient ctx = role ctx == Client

isServer :: Context -> Bool
isServer ctx = role ctx == Server

----------------------------------------------------------------

getMyNewStreamId :: Context -> STM StreamId
getMyNewStreamId Context{..} = do
    n <- readTVar myStreamId
    let n' = n + 2
    writeTVar myStreamId n'
    return n

getPeerStreamID :: Context -> IO StreamId
getPeerStreamID ctx = readIORef $ peerStreamId ctx

setPeerStreamID :: Context -> StreamId -> IO ()
setPeerStreamID ctx sid = writeIORef (peerStreamId ctx) sid

----------------------------------------------------------------

{-# INLINE setStreamState #-}
setStreamState :: Context -> Stream -> StreamState -> IO ()
setStreamState _ Stream{streamState} val = writeIORef streamState val

opened :: Context -> Stream -> IO ()
opened ctx strm = setStreamState ctx strm (Open Nothing JustOpened)

halfClosedRemote :: Context -> Stream -> IO ()
halfClosedRemote ctx stream@Stream{streamState} = do
    closingCode <- atomicModifyIORef streamState closeHalf
    traverse_ (closed ctx stream) closingCode
  where
    closeHalf :: StreamState -> (StreamState, Maybe ClosedCode)
    closeHalf x@(Closed _) = (x, Nothing)
    closeHalf (Open (Just cc) _) = (Closed cc, Just cc)
    closeHalf _ = (HalfClosedRemote, Nothing)

halfClosedLocal :: Context -> Stream -> ClosedCode -> IO ()
halfClosedLocal ctx stream@Stream{streamState} cc = do
    shouldFinalize <- atomicModifyIORef streamState closeHalf
    when shouldFinalize $
        closed ctx stream cc
  where
    closeHalf :: StreamState -> (StreamState, Bool)
    closeHalf x@(Closed _) = (x, False)
    closeHalf HalfClosedRemote = (Closed cc, True)
    closeHalf (Open Nothing o) = (Open (Just cc) o, False)
    closeHalf _ = (Open (Just cc) JustOpened, False)

closed :: Context -> Stream -> ClosedCode -> IO ()
closed ctx@Context{oddStreamTable, evenStreamTable} strm@Stream{streamNumber} cc = do
    if isServerInitiated streamNumber
        then deleteEven evenStreamTable streamNumber err
        else deleteOdd oddStreamTable streamNumber err
    setStreamState ctx strm (Closed cc) -- anyway
  where
    err :: SomeException
    err = toException (closedCodeToError streamNumber cc)

----------------------------------------------------------------
-- From peer

-- Server
openOddStreamCheck :: Context -> StreamId -> FrameType -> IO Stream
openOddStreamCheck ctx@Context{oddStreamTable, peerSettings, mySettings} sid ftyp = do
    -- My SETTINGS_MAX_CONCURRENT_STREAMS
    when (ftyp == FrameHeaders) $ do
        conc <- getOddConcurrency oddStreamTable
        checkMyConcurrency sid mySettings conc
    txws <- initialWindowSize <$> readIORef peerSettings
    rxws <- initialWindowSize <$> readIORef mySettings
    newstrm <- newOddStream sid txws rxws
    when (ftyp == FrameHeaders || ftyp == FramePushPromise) $ opened ctx newstrm
    insertOdd oddStreamTable sid newstrm
    return newstrm

-- Client
openEvenStreamCacheCheck :: Context -> StreamId -> Method -> ByteString -> IO ()
openEvenStreamCacheCheck Context{evenStreamTable, peerSettings, mySettings} sid method path = do
    -- My SETTINGS_MAX_CONCURRENT_STREAMS
    conc <- getEvenConcurrency evenStreamTable
    checkMyConcurrency sid mySettings conc
    txws <- initialWindowSize <$> readIORef peerSettings
    rxws <- initialWindowSize <$> readIORef mySettings
    newstrm <- newEvenStream sid txws rxws
    insertEvenCache evenStreamTable method path newstrm

checkMyConcurrency
    :: StreamId -> IORef Settings -> Int -> IO ()
checkMyConcurrency sid settings conc = do
    mMaxConc <- maxConcurrentStreams <$> readIORef settings
    case mMaxConc of
        Nothing -> return ()
        Just maxConc ->
            when (conc >= maxConc) $
                E.throwIO $
                    StreamErrorIsSent RefusedStream sid "exceeds max concurrent"

----------------------------------------------------------------
-- From me

-- Clinet
openOddStreamWait :: Context -> IO (StreamId, Stream)
openOddStreamWait ctx@Context{oddStreamTable, mySettings, peerSettings} = do
    -- Peer SETTINGS_MAX_CONCURRENT_STREAMS
    mMaxConc <- maxConcurrentStreams <$> readIORef peerSettings
    case mMaxConc of
        Nothing -> do
            sid <- atomically $ getMyNewStreamId ctx
            txws <- initialWindowSize <$> readIORef peerSettings
            rxws <- initialWindowSize <$> readIORef mySettings
            newstrm <- newOddStream sid txws rxws
            insertOdd oddStreamTable sid newstrm
            return (sid, newstrm)
        Just maxConc -> do
            sid <- atomically $ do
                waitIncOdd oddStreamTable maxConc
                getMyNewStreamId ctx
            txws <- initialWindowSize <$> readIORef peerSettings
            rxws <- initialWindowSize <$> readIORef mySettings
            newstrm <- newOddStream sid txws rxws
            insertOdd' oddStreamTable sid newstrm
            return (sid, newstrm)

-- Server
openEvenStreamWait :: Context -> IO (StreamId, Stream)
openEvenStreamWait ctx@Context{..} = do
    -- Peer SETTINGS_MAX_CONCURRENT_STREAMS
    mMaxConc <- maxConcurrentStreams <$> readIORef peerSettings
    case mMaxConc of
        Nothing -> do
            sid <- atomically $ getMyNewStreamId ctx
            txws <- initialWindowSize <$> readIORef peerSettings
            rxws <- initialWindowSize <$> readIORef mySettings
            newstrm <- newEvenStream sid txws rxws
            insertEven evenStreamTable sid newstrm
            return (sid, newstrm)
        Just maxConc -> do
            sid <- atomically $ do
                waitIncEven evenStreamTable maxConc
                getMyNewStreamId ctx
            txws <- initialWindowSize <$> readIORef peerSettings
            rxws <- initialWindowSize <$> readIORef mySettings
            newstrm <- newEvenStream sid txws rxws
            insertEven' evenStreamTable sid newstrm
            return (sid, newstrm)
