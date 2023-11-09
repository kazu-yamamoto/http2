{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Arch.Stream where

import Control.Exception
import Data.IORef
import UnliftIO.Concurrent
import UnliftIO.STM

import Network.HTTP2.Arch.StreamTable
import Network.HTTP2.Arch.Types
import Network.HTTP2.Frame

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

newOddStream :: StreamId -> WindowSize -> IO Stream
newOddStream sid win =
    Stream sid
        <$> newIORef Idle
        <*> newTVarIO win
        <*> newEmptyMVar

newEvenStream :: StreamId -> WindowSize -> IO Stream
newEvenStream sid win =
    Stream sid
        <$> newIORef Reserved
        <*> newTVarIO win
        <*> newEmptyMVar

----------------------------------------------------------------

{-# INLINE readStreamState #-}
readStreamState :: Stream -> IO StreamState
readStreamState Stream{streamState} = readIORef streamState

----------------------------------------------------------------

closeAllStreams
    :: IORef OddStreamTable -> IORef EvenStreamTable -> Maybe SomeException -> IO ()
closeAllStreams oref eref mErr' = do
    ostrms <-
        oddTable <$> atomicModifyIORef' oref (\st -> (emptyOddStreamTable, st))
    mapM_ finalize ostrms
    estrms <-
        evenTable <$> atomicModifyIORef' eref (\st -> (emptyEvenStreamTable 0, st))
    mapM_ finalize estrms
  where
    finalize strm = do
        st <- readStreamState strm
        case st of
            Open _ (Body q _ _ _) ->
                atomically $ writeTQueue q $ maybe (Right mempty) Left mErr
            _otherwise ->
                return ()
    mErr :: Maybe SomeException
    mErr = case mErr' of
        Just err
            | Just ConnectionIsClosed <- fromException err ->
                Nothing
        _otherwise ->
            mErr'
