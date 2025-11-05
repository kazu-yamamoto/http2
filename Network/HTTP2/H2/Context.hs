{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Context where

import Control.Concurrent.STM
import Control.Exception
import qualified Control.Exception as E
import Data.IORef
import Network.Control
import Network.Socket (SockAddr)
import qualified System.ThreadManager as T

import Imports hiding (insert)
import Network.HPACK
import Network.HTTP2.Frame
import Network.HTTP2.H2.Settings
import Network.HTTP2.H2.Stream
import Network.HTTP2.H2.StreamTable
import Network.HTTP2.H2.Types

data Role = Client | Server deriving (Eq, Show)

----------------------------------------------------------------

data RoleInfo = RIS ServerInfo | RIC ClientInfo

type Launch = Context -> Stream -> InpObj -> IO ()

newtype ServerInfo = ServerInfo
    { launch :: Launch
    }

data ClientInfo = ClientInfo
    { scheme :: ByteString
    , authority :: Authority
    }

toServerInfo :: RoleInfo -> ServerInfo
toServerInfo (RIS x) = x
toServerInfo _ = error "toServerInfo"

toClientInfo :: RoleInfo -> ClientInfo
toClientInfo (RIC x) = x
toClientInfo _ = error "toClientInfo"

newServerInfo :: Launch -> RoleInfo
newServerInfo = RIS . ServerInfo

newClientInfo :: ByteString -> Authority -> RoleInfo
newClientInfo scm auth = RIC $ ClientInfo scm auth

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
-- | The context for HTTP/2 connection.
data Context = Context
    { role               :: Role
    , roleInfo           :: RoleInfo
    , -- Settings
      mySettings         :: Settings
    , myFirstSettings    :: IORef Bool
    , peerSettings       :: IORef Settings
    , oddStreamTable     :: TVar OddStreamTable
    , evenStreamTable    :: TVar EvenStreamTable
    , continued          :: IORef (Maybe StreamId)
    -- ^ RFC 9113 says "Other frames (from any stream) MUST NOT
    --   occur between the HEADERS frame and any CONTINUATION
    --   frames that might follow". This field is used to implement
    --   this requirement.
    , myStreamId         :: TVar StreamId
    , peerStreamId       :: IORef StreamId
    , peerLastStreamId   :: IORef StreamId
    , outputBufferLimit  :: IORef Int
    , outputQ            :: TQueue Output
    -- ^ Invariant: Each stream will only ever have at most one 'Output'
    -- object in this queue at any moment.
    , outputQStreamID    :: TVar StreamId
    , controlQ           :: TQueue Control
    , encodeDynamicTable :: DynamicTable
    , decodeDynamicTable :: DynamicTable
    , -- the connection window for sending data
      txFlow             :: TVar TxFlow
    , rxFlow             :: IORef RxFlow
    , pingRate           :: Rate
    , settingsRate       :: Rate
    , emptyFrameRate     :: Rate
    , rstRate            :: Rate
    , mySockAddr         :: SockAddr
    , peerSockAddr       :: SockAddr
    , threadManager      :: T.ThreadManager
    }
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
newContext
    :: RoleInfo
    -> Config
    -> Int
    -> Int
    -> Settings
    -> T.Manager
    -> IO Context
newContext roleInfo Config{..} cacheSiz connRxWS mySettings timmgr = do
    -- My: Use this even if ack has not been received yet.
    myFirstSettings <- newIORef False
    -- Peer: The spec defines max concurrency is infinite unless
    -- SETTINGS_MAX_CONCURRENT_STREAMS is exchanged.
    -- But it is vulnerable, so we set the limitations.
    peerSettings <-
        newIORef baseSettings{maxConcurrentStreams = Just defaultMaxStreams}
    oddStreamTable    <- newTVarIO emptyOddStreamTable
    evenStreamTable   <- newTVarIO (emptyEvenStreamTable cacheSiz)
    continued         <- newIORef Nothing
    myStreamId        <- newTVarIO sid0
    peerStreamId      <- newIORef 0
    peerLastStreamId  <- newIORef 0
    outputBufferLimit <- newIORef buflim
    outputQ           <- newTQueueIO
    outputQStreamID   <- newTVarIO sid0
    controlQ          <- newTQueueIO
    -- My SETTINGS_HEADER_TABLE_SIZE
    encodeDynamicTable <- newDynamicTableForEncoding defaultDynamicTableSize
    decodeDynamicTable <-
        newDynamicTableForDecoding (headerTableSize mySettings) 4096
    txFlow          <- newTVarIO (newTxFlow defaultWindowSize) -- 64K
    rxFlow          <- newIORef (newRxFlow connRxWS)
    pingRate        <- newRate
    settingsRate    <- newRate
    emptyFrameRate  <- newRate
    rstRate         <- newRate
    let mySockAddr   = confMySockAddr
    let peerSockAddr = confPeerSockAddr
    threadManager   <- T.newThreadManager timmgr
    return Context{..}
  where
    role = case roleInfo of
        RIC{} -> Client
        _ -> Server
    sid0
        | role == Client = 1
        | otherwise = 2
    dlim = defaultPayloadLength + frameHeaderLength
    buflim
        | confBufferSize >= dlim = dlim
        | otherwise = confBufferSize
{- FOURMOLU_ENABLE -}

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

getPeerLastStreamId :: Context -> IO StreamId
getPeerLastStreamId ctx = readIORef $ peerLastStreamId ctx

modifyPeerLastStreamId :: Context -> StreamId -> IO ()
modifyPeerLastStreamId ctx sid = atomicModifyIORef' (peerLastStreamId ctx) $ \n -> if sid > n then (sid, ()) else (n, ())

----------------------------------------------------------------

{-# INLINE setStreamState #-}
setStreamState :: Context -> Stream -> StreamState -> IO ()
setStreamState _ Stream{streamState} newState = do
    oldState <- readIORef streamState
    case (oldState, newState) of
        (Open _ (Body q _ _ _), Open _ (Body q' _ _ _))
            | q == q' ->
                -- The stream stays open with the same body; nothing to do
                return ()
        (Open _ (Body q _ _ _), _) ->
            -- The stream is either closed, or is open with a /new/ body
            -- We need to close the old queue so that any reads from it won't block
            atomically $ writeTQueue q $ Left $ toException ConnectionIsClosed
        _otherwise ->
            -- The stream wasn't open to start with; nothing to do
            return ()
    writeIORef streamState newState

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
        checkMyConcurrency sid mySettings (conc + 1)
    txws <- initialWindowSize <$> readIORef peerSettings
    let rxws = initialWindowSize mySettings
    newstrm <- newOddStream sid txws rxws
    when (ftyp == FrameHeaders || ftyp == FramePushPromise) $ opened ctx newstrm
    insertOdd oddStreamTable sid newstrm
    return newstrm

-- Client
openEvenStreamCacheCheck :: Context -> StreamId -> Method -> ByteString -> IO ()
openEvenStreamCacheCheck Context{evenStreamTable, peerSettings, mySettings} sid method path = do
    -- My SETTINGS_MAX_CONCURRENT_STREAMS
    conc <- getEvenConcurrency evenStreamTable
    checkMyConcurrency sid mySettings (conc + 1)
    txws <- initialWindowSize <$> readIORef peerSettings
    let rxws = initialWindowSize mySettings
    newstrm <- newEvenStream sid txws rxws
    insertEvenCache evenStreamTable method path newstrm

checkMyConcurrency
    :: StreamId -> Settings -> Int -> IO ()
checkMyConcurrency sid settings conc = do
    let mMaxConc = maxConcurrentStreams settings
    case mMaxConc of
        Nothing -> return ()
        Just maxConc ->
            when (conc > maxConc) $
                E.throwIO $
                    StreamErrorIsSent RefusedStream sid "exceeds max concurrent"

----------------------------------------------------------------
-- From me

-- Clinet
openOddStreamWait :: Context -> IO (StreamId, Stream)
openOddStreamWait ctx@Context{oddStreamTable, mySettings, peerSettings} = do
    -- Peer SETTINGS_MAX_CONCURRENT_STREAMS
    mMaxConc <- maxConcurrentStreams <$> readIORef peerSettings
    let rxws = initialWindowSize mySettings
    case mMaxConc of
        Nothing -> do
            sid <- atomically $ getMyNewStreamId ctx
            txws <- initialWindowSize <$> readIORef peerSettings
            newstrm <- newOddStream sid txws rxws
            insertOdd oddStreamTable sid newstrm
            return (sid, newstrm)
        Just maxConc -> do
            sid <- atomically $ do
                waitIncOdd oddStreamTable maxConc
                getMyNewStreamId ctx
            txws <- initialWindowSize <$> readIORef peerSettings
            newstrm <- newOddStream sid txws rxws
            insertOdd' oddStreamTable sid newstrm
            return (sid, newstrm)

-- Server
openEvenStreamWait :: Context -> IO (StreamId, Stream)
openEvenStreamWait ctx@Context{..} = do
    -- Peer SETTINGS_MAX_CONCURRENT_STREAMS
    mMaxConc <- maxConcurrentStreams <$> readIORef peerSettings
    let rxws = initialWindowSize mySettings
    case mMaxConc of
        Nothing -> do
            sid <- atomically $ getMyNewStreamId ctx
            txws <- initialWindowSize <$> readIORef peerSettings
            newstrm <- newEvenStream sid txws rxws
            insertEven evenStreamTable sid newstrm
            return (sid, newstrm)
        Just maxConc -> do
            sid <- atomically $ do
                waitIncEven evenStreamTable maxConc
                getMyNewStreamId ctx
            txws <- initialWindowSize <$> readIORef peerSettings
            newstrm <- newEvenStream sid txws rxws
            insertEven' evenStreamTable sid newstrm
            return (sid, newstrm)
