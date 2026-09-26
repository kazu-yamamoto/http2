{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Context where

import Control.Concurrent.STM
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
    , continued          :: IORef (Maybe HeaderContinuation)
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
    , receiverDone       :: TVar (Maybe E.SomeException)
    , workersDone        :: STM Bool
    , informationalCallback :: StreamId -> TokenHeaderTable -> IO ()
    -- ^ Client only: called when a 1xx informational response (e.g. 103 Early
    --   Hints) is received, ahead of the final response. Copied from
    --   'confOnInformational'; no-op by default.
    }
{- FOURMOLU_ENABLE -}

-- | Header/trailer continuation
--
-- RFC 9113 says "Other frames (from any stream) MUST NOT occur between the
-- HEADERS frame and any CONTINUATION frames that might follow". This is used to
-- implement this requirement.
--
-- It also accumulates the fragments of the block. These are connection-level
-- state: the block must be decoded even if its stream is reset before the
-- block is complete, since it may modify the dynamic table.
data HeaderContinuation = HeaderContinuation
    { hcStreamId :: StreamId
    , hcBlock :: PartialHeaderBlock
    , hcEndOfStream :: Bool
    -- ^ END_STREAM, from the HEADERS frame that started the block
    }

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
newContext
    :: RoleInfo
    -> Config
    -> Int
    -> Int
    -> Settings
    -> T.Manager
    -> Maybe (STM Bool)
    -> IO Context
newContext roleInfo Config{..} cacheSiz connRxWS mySettings timmgr mdone = do
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
    receiverDone    <- newTVarIO Nothing
    let informationalCallback = confOnInformational
    let workersDone = fromMaybe (T.isAllGone threadManager) mdone
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
setStreamState _ Stream{streamNumber, streamState} newState = atomically $ do
    oldState <- readTVar streamState
    informReplaced streamNumber oldState newState
    writeTVar streamState newState

-- | Replacing the open state of a stream as the receiver moves it on, from
-- headers to body.
--
-- The receiver reads a stream's state, works out the next one from the
-- frame, and writes it back -- in a transaction of its own.  In between, the
-- sender may have half-closed the stream on our side ('halfClosedLocal',
-- which records it as @Open (Just cc) _@) or closed it.  Writing the whole
-- state back undid that: the half-close was lost, the peer's END_STREAM then
-- took the stream to half-closed (remote) rather than closed, and it stayed
-- in the stream table, holding its concurrency slot for good.  With both ends
-- streaming at once -- gRPC-style -- a client ran out of streams and a
-- server refused every new one.
--
-- So only the open state is replaced, keeping whatever the sender recorded
-- about our side, and a stream that is no longer open is left alone.
setOpenState :: Context -> Stream -> OpenState -> IO ()
setOpenState _ Stream{streamNumber, streamState} o = atomically $ do
    oldState <- readTVar streamState
    case oldState of
        Open hcl _ -> do
            let newState = Open hcl o
            informReplaced streamNumber oldState newState
            writeTVar streamState newState
        _otherwise -> return ()

-- | Inform consumers of any streams that we close
informReplaced :: StreamId -> StreamState -> StreamState -> STM ()
informReplaced streamNumber oldState newState =
    case (oldState, newState) of
        (Open _ (Body q _ _ _), Open _ (Body q' _ _ _))
            | q == q' ->
                -- The stream stays open with the same body; nothing to do
                return ()
        (Open _ (Body q _ _ _), Closed cc) ->
            writeTQueue q $ Left $ E.toException $ closedCodeToError streamNumber cc
        (Open _ (Body q _ _ _), _) ->
            -- The stream is opened with a /new/ body
            writeTQueue q $ Left $ E.toException ConnectionIsClosed
        _otherwise ->
            -- The stream wasn't open to start with; nothing to do
            return ()

opened :: Context -> Stream -> IO ()
opened ctx strm = setStreamState ctx strm (Open Nothing JustOpened)

halfClosedRemote :: Context -> Stream -> IO ()
halfClosedRemote ctx stream@Stream{streamState} = do
    closingCode <- atomically $ stateTVar streamState closeHalf
    traverse_ (closed ctx stream) closingCode
  where
    closeHalf :: StreamState -> (Maybe ClosedCode, StreamState)
    closeHalf x@(Closed _) = (Nothing, x)
    closeHalf (Open (Just cc) _) = (Just cc, Closed cc)
    closeHalf _ = (Nothing, HalfClosedRemote)

halfClosedLocal :: Context -> Stream -> ClosedCode -> IO ()
halfClosedLocal ctx stream@Stream{streamState} cc = do
    shouldFinalize <- atomically $ stateTVar streamState closeHalf
    when shouldFinalize $
        closed ctx stream cc
  where
    closeHalf :: StreamState -> (Bool, StreamState)
    closeHalf x@(Closed _) = (False, x)
    closeHalf HalfClosedRemote = (True, Closed cc)
    closeHalf (Open Nothing o) = (False, Open (Just cc) o)
    closeHalf _ = (False, Open (Just cc) JustOpened)

closed :: Context -> Stream -> ClosedCode -> IO ()
closed ctx@Context{oddStreamTable, evenStreamTable} strm@Stream{streamNumber} cc = do
    if isServerInitiated streamNumber
        then deleteEven evenStreamTable streamNumber err
        else deleteOdd oddStreamTable streamNumber err
    setStreamState ctx strm (Closed cc) -- anyway
  where
    err :: E.SomeException
    err = E.toException (closedCodeToError streamNumber cc)

----------------------------------------------------------------
-- From peer

-- Server
--
-- Note that this does not apply SETTINGS_MAX_CONCURRENT_STREAMS.  A stream
-- over the limit still has to be admitted this far, because its field block
-- has to be decoded before it can be refused; 'checkOddConcurrency' does the
-- refusing once that has happened.
openOddStreamCheck :: Context -> StreamId -> FrameType -> IO Stream
openOddStreamCheck ctx@Context{oddStreamTable, peerSettings, mySettings} sid ftyp = do
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

-- | Refuse a peer-initiated stream that puts us over the limit we advertised
-- in SETTINGS_MAX_CONCURRENT_STREAMS.
--
-- Checked once the stream's field block has been decoded, rather than when
-- its HEADERS frame arrived.  A block has to be decoded whatever becomes of
-- its stream -- RFC 9113 section 10.5.1, "The field block MUST be processed
-- to ensure a consistent connection state" -- and refusing at arrival meant
-- throwing before the frame's payload had even been read, which left nothing
-- to do but drop the connection.  From here the throw lands inside the
-- receiver's per-frame reset handler, so the answer is
-- RST_STREAM(REFUSED_STREAM) and the connection carries on, which is what
-- section 5.1.2 asks for and what section 8.7 lets the peer retry against.
--
-- The stream is in the table by the time we get here, so it counts itself.
checkOddConcurrency :: Context -> StreamId -> IO ()
checkOddConcurrency Context{oddStreamTable, mySettings} sid = do
    conc <- getOddConcurrency oddStreamTable
    checkMyConcurrency sid mySettings conc

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
