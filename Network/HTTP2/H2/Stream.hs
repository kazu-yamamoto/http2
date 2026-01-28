{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Stream where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Maybe (fromMaybe)
import Network.Control
import Network.HTTP.Semantics
import Network.HTTP.Semantics.IO

import Network.HTTP2.Frame
import Network.HTTP2.H2.StreamTable
import Network.HTTP2.H2.Types

----------------------------------------------------------------

isIdle :: StreamState -> Bool
isIdle Idle = True
isIdle _ = False

isOpen :: StreamState -> Bool
isOpen Open{} = True
isOpen _ = False

isHalfClosedRemote :: StreamState -> Bool
isHalfClosedRemote HalfClosedRemote = True
isHalfClosedRemote (Closed _) = True
isHalfClosedRemote _ = False

isHalfClosedLocal :: StreamState -> Bool
isHalfClosedLocal (Open (Just _) _) = True
isHalfClosedLocal (Closed _) = True
isHalfClosedLocal _ = False

isClosed :: StreamState -> Bool
isClosed Closed{} = True
isClosed _ = False

isReserved :: StreamState -> Bool
isReserved Reserved = True
isReserved _ = False

----------------------------------------------------------------

newOddStream :: StreamId -> WindowSize -> WindowSize -> IO Stream
newOddStream sid txwin rxwin =
    Stream sid
        <$> newTVarIO Idle
        <*> newEmptyMVar
        <*> newTVarIO (newTxFlow txwin)
        <*> newIORef (newRxFlow rxwin)
        <*> newIORef Nothing

newEvenStream :: StreamId -> WindowSize -> WindowSize -> IO Stream
newEvenStream sid txwin rxwin =
    Stream sid
        <$> newTVarIO Reserved
        <*> newEmptyMVar
        <*> newTVarIO (newTxFlow txwin)
        <*> newIORef (newRxFlow rxwin)
        <*> newIORef Nothing

----------------------------------------------------------------

{-# INLINE readStreamState #-}
readStreamState :: Stream -> IO StreamState
readStreamState Stream{streamState} = readTVarIO streamState

----------------------------------------------------------------

closeAllStreams
    :: TVar OddStreamTable -> TVar EvenStreamTable -> Maybe SomeException -> IO ()
closeAllStreams ovar evar mErr' = do
    ostrms <- clearOddStreamTable ovar
    mapM_ finalize ostrms
    estrms <- clearEvenStreamTable evar
    mapM_ finalize estrms
  where
    finalize strm = do
        st <- readStreamState strm
        void $ tryPutMVar (streamInput strm) err
        case st of
            Open _ (Body q _ _ _) ->
                atomically $ writeTQueue q $ maybe (Right (mempty, True)) Left mErr
            _otherwise ->
                return ()

    mErr :: Maybe SomeException
    mErr = case mErr' of
        Just e
            | Just ConnectionIsClosed <- fromException e ->
                Nothing
        _otherwise ->
            mErr'

    err :: Either SomeException a
    err =
        Left $
            fromMaybe (toException ConnectionIsClosed) $
                mErr

----------------------------------------------------------------

data StreamTerminated
    = StreamPushedFinal
    | StreamCancelled
    | StreamOutOfScope
    | StreamRemoteReset ClosedCode
    deriving (Show)
    deriving anyclass (Exception)

withOutBodyIface
    :: Stream
    -> (Maybe SomeException -> STM ())
    -> TBQueue StreamingChunk
    -> (forall a. IO a -> IO a)
    -> (OutBodyIface -> IO r)
    -> IO r
withOutBodyIface stream cancelAfterFinish tbq unmask k = do
    terminated <- newTVarIO Nothing

    let checkNotTerminated :: STM ()
        checkNotTerminated = do
            mTerminated <- readTVar terminated
            case mTerminated of
                Just reason ->
                    throwSTM reason
                Nothing ->
                    return ()

        getIsClosed :: STM (Maybe ClosedCode)
        getIsClosed = do
            st <- readTVar (streamState stream)
            case st of
                Closed code -> return $ Just code
                _otherwise -> return Nothing

        -- Check if the peer is still listening for messages
        --
        -- It is important to call 'checkNotClosed' prior to enqueuing stream
        -- chunks to ensure that 'writeTBQueue' will not block indefinitely
        -- (because nothing is consuming elements from the queue anymore).
        --
        -- Assumes 'checkNotTerminated'.
        checkNotClosed :: STM ()
        checkNotClosed = do
            mClosed <- getIsClosed
            case mClosed of
                Just code ->
                    -- When the stream is closed, but /we/ did not close it (or
                    -- 'checkNotTerminated' would have thrown an exception), it
                    -- must mean that our peer send us a RST_STREAM, indicating
                    -- that they do not want to receive any further messages.
                    throwSTM $ StreamRemoteReset code
                _otherwise ->
                    return ()

        iface :: OutBodyIface
        iface =
            OutBodyIface
                { outBodyUnmask = unmask
                , outBodyPush = \b -> atomically $ do
                    checkNotTerminated
                    checkNotClosed
                    writeTBQueue tbq $ StreamingBuilder b NotEndOfStream
                , outBodyPushFinal = \b -> atomically $ do
                    checkNotTerminated
                    checkNotClosed
                    writeTVar terminated (Just StreamPushedFinal)
                    writeTBQueue tbq $ StreamingBuilder b (EndOfStream Nothing)
                    writeTBQueue tbq $ StreamingFinished Nothing
                , outBodyFlush = atomically $ do
                    checkNotTerminated
                    checkNotClosed
                    writeTBQueue tbq StreamingFlush
                , outBodyCancel = \mErr -> atomically $ do
                    mTerminated <- readTVar terminated
                    mClosed <- getIsClosed
                    case (mClosed, mTerminated) of
                        (Nothing, Nothing) -> do
                            writeTVar terminated (Just StreamCancelled)
                            writeTBQueue tbq (StreamingCancelled mErr)
                        (Nothing, Just StreamCancelled) ->
                            -- Already cancelled
                            return ()
                        (Nothing, Just _) -> do
                            -- We finished streaming (that is, sending messages to the peer),
                            -- but we must still be able to cancel the stream entirely
                            -- (that is, tell the peer that we no longer want to /receive/ messages: RST_STREAM)
                            writeTVar terminated (Just StreamCancelled)
                            cancelAfterFinish mErr
                        (Just _code, _) ->
                            -- Peer already closed
                            return ()
                }

        finished :: IO ()
        finished = atomically $ do
            mTerminated <- readTVar terminated
            mClosed <- getIsClosed
            case (mClosed, mTerminated) of
                (Nothing, Nothing) -> do
                    writeTVar terminated (Just StreamOutOfScope)
                    writeTBQueue tbq $ StreamingFinished Nothing
                (Nothing, Just _) ->
                    -- We already terminated
                    return ()
                (Just _code, _) ->
                    -- Peer already closed
                    return ()

    k iface `finally` finished

nextForStreaming
    :: TBQueue StreamingChunk
    -> DynaNext
nextForStreaming tbq =
    let takeQ = atomically $ tryReadTBQueue tbq
        next = fillStreamBodyGetNext takeQ
     in next
