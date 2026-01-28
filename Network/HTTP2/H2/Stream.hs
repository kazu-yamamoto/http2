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
        <$> newIORef Idle
        <*> newEmptyMVar
        <*> newTVarIO (newTxFlow txwin)
        <*> newIORef (newRxFlow rxwin)
        <*> newIORef Nothing

newEvenStream :: StreamId -> WindowSize -> WindowSize -> IO Stream
newEvenStream sid txwin rxwin =
    Stream sid
        <$> newIORef Reserved
        <*> newEmptyMVar
        <*> newTVarIO (newTxFlow txwin)
        <*> newIORef (newRxFlow rxwin)
        <*> newIORef Nothing

----------------------------------------------------------------

{-# INLINE readStreamState #-}
readStreamState :: Stream -> IO StreamState
readStreamState Stream{streamState} = readIORef streamState

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
    deriving (Show)
    deriving anyclass (Exception)

withOutBodyIface
    :: TBQueue StreamingChunk
    -> (forall a. IO a -> IO a)
    -> (OutBodyIface -> IO r)
    -> IO r
withOutBodyIface tbq unmask k = do
    terminated <- newTVarIO Nothing

    let checkNotTerminated :: STM ()
        checkNotTerminated = do
            mTerminated <- readTVar terminated
            case mTerminated of
                Just reason ->
                    throwSTM reason
                Nothing ->
                    return ()

        iface :: OutBodyIface
        iface =
            OutBodyIface
                { outBodyUnmask = unmask
                , outBodyPush = \b -> atomically $ do
                    checkNotTerminated
                    writeTBQueue tbq $ StreamingBuilder b NotEndOfStream
                , outBodyPushFinal = \b -> atomically $ do
                    checkNotTerminated
                    writeTVar terminated (Just StreamPushedFinal)
                    writeTBQueue tbq $ StreamingBuilder b (EndOfStream Nothing)
                    writeTBQueue tbq $ StreamingFinished Nothing
                , outBodyFlush = atomically $ do
                    checkNotTerminated
                    writeTBQueue tbq StreamingFlush
                , outBodyCancel = \mErr -> atomically $ do
                    mTerminated <- readTVar terminated
                    case mTerminated of
                        Nothing -> do
                            writeTVar terminated (Just StreamCancelled)
                            writeTBQueue tbq (StreamingCancelled mErr)
                        Just _ ->
                            -- Already terminated
                            return ()
                }

        finished :: IO ()
        finished = atomically $ do
            mTerminated <- readTVar terminated
            case mTerminated of
                Nothing -> do
                    writeTVar terminated (Just StreamOutOfScope)
                    writeTBQueue tbq $ StreamingFinished Nothing
                Just _ ->
                    -- Already terminated
                    return ()

    k iface `finally` finished

nextForStreaming
    :: TBQueue StreamingChunk
    -> DynaNext
nextForStreaming tbq =
    let takeQ = atomically $ tryReadTBQueue tbq
        next = fillStreamBodyGetNext takeQ
     in next
