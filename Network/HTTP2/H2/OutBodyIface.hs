{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Network.HTTP2.H2.OutBodyIface (
    StreamTerminated (..),
    withOutBodyIface,
) where

import Control.Concurrent.STM
import Control.Exception
import Network.HTTP.Semantics
import Network.HTTP.Semantics.IO
import Network.HTTP2.H2.Context
import Network.HTTP2.H2.Sync
import Network.HTTP2.H2.Types

----------------------------------------------------------------

data StreamTerminated
    = StreamPushedFinal
    | StreamCancelled
    | StreamOutOfScope
    | StreamRemoteReset ClosedCode
    deriving (Show)
    deriving anyclass (Exception)

----------------------------------------------------------------

withOutBodyIface
    :: Context
    -> Stream
    -> TBQueue StreamingChunk
    -> (forall a. IO a -> IO a)
    -> (OutBodyIface -> IO r)
    -> IO r
withOutBodyIface ctx@Context{outputQ} strm tbq unmask k = do
    terminated <- newTVarIO Nothing
    let checkNotTerminated :: STM ()
        checkNotTerminated = do
            mTerminated <- readTVar terminated
            maybe (return ()) throwSTM mTerminated

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

        getIsClosed :: STM (Maybe ClosedCode)
        getIsClosed = do
            st <- readTVar (streamState strm)
            case st of
                Closed code -> return $ Just code
                _otherwise -> return Nothing

        cancelAfterFinish :: Maybe SomeException -> STM ()
        cancelAfterFinish mErr =
            writeTQueue outputQ $ makeOutputIO ctx strm Nothing (OReset mErr)

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
                            writeTBQueue tbq $ StreamingCancelled mErr
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
