{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Arch.Stream where

import Control.Exception
import Data.IORef
import qualified Data.IntMap.Strict as M
import UnliftIO.Concurrent
import UnliftIO.STM

import Imports
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

----------------------------------------------------------------

newStream :: StreamId -> WindowSize -> IO Stream
newStream sid win =
    Stream sid
        <$> newIORef Idle
        <*> newTVarIO win
        <*> newEmptyMVar

newPushStream :: StreamId -> WindowSize -> IO Stream
newPushStream sid win =
    Stream sid
        <$> newIORef Reserved
        <*> newTVarIO win
        <*> newEmptyMVar

----------------------------------------------------------------

{-# INLINE readStreamState #-}
readStreamState :: Stream -> IO StreamState
readStreamState Stream{streamState} = readIORef streamState

----------------------------------------------------------------

emptyStreamTable :: StreamTable
emptyStreamTable = StreamTable 0 M.empty

insert :: IORef StreamTable -> M.Key -> Stream -> IO ()
insert ref k v = atomicModifyIORef' ref $ \StreamTable{..} ->
    let concurrency' = concurrency + 1
        streams' = M.insert k v streams
     in (StreamTable concurrency' streams', ())

remove :: IORef StreamTable -> M.Key -> IO ()
remove ref k = atomicModifyIORef' ref $ \StreamTable{..} ->
    let concurrency' = concurrency - 1
        streams' = M.delete k streams
     in (StreamTable concurrency' streams', ())

search :: IORef StreamTable -> M.Key -> IO (Maybe Stream)
search ref k = M.lookup k . streams <$> readIORef ref

updateAllStreamWindow
    :: (WindowSize -> WindowSize) -> IORef StreamTable -> IO ()
updateAllStreamWindow adst ref = do
    strms <- streams <$> readIORef ref
    forM_ strms $ \strm -> atomically $ modifyTVar (streamWindow strm) adst

closeAllStreams :: IORef StreamTable -> Maybe SomeException -> IO ()
closeAllStreams ref mErr' = do
    strms <- streams <$> atomicModifyIORef' ref (\st -> (emptyStreamTable, st))
    forM_ strms $ \strm -> do
        st <- readStreamState strm
        case st of
            Open _ (Body q _ _ _) ->
                atomically $ writeTQueue q $ maybe (Right mempty) Left mErr
            _otherwise ->
                return ()
  where
    mErr :: Maybe SomeException
    mErr = case mErr' of
        Just err
            | Just ConnectionIsClosed <- fromException err ->
                Nothing
        _otherwise ->
            mErr'
