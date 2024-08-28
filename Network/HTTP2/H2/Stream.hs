{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Stream where

import Control.Monad
import Data.IORef
import Data.Maybe (fromMaybe)
import Network.Control
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.STM

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
