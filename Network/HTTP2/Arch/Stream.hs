{-# LANGUAGE NamedFieldPuns #-}

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
isIdle _    = False

isOpen :: StreamState -> Bool
isOpen Open{} = True
isOpen _      = False

isHalfClosedRemote :: StreamState -> Bool
isHalfClosedRemote HalfClosedRemote = True
isHalfClosedRemote (Closed _)       = True
isHalfClosedRemote _                = False

isHalfClosedLocal :: StreamState -> Bool
isHalfClosedLocal (Open (Just _) _) = True
isHalfClosedLocal (Closed _)        = True
isHalfClosedLocal _                 = False

isClosed :: StreamState -> Bool
isClosed Closed{} = True
isClosed _        = False

----------------------------------------------------------------

newStream :: StreamId -> WindowSize -> IO Stream
newStream sid win = Stream sid <$> newIORef Idle
                               <*> newTVarIO win
                               <*> newEmptyMVar

newPushStream :: StreamId -> WindowSize -> IO Stream
newPushStream sid win = Stream sid <$> newIORef Reserved
                                   <*> newTVarIO win
                                   <*> newEmptyMVar

----------------------------------------------------------------

{-# INLINE readStreamState #-}
readStreamState :: Stream -> IO StreamState
readStreamState Stream{streamState} = readIORef streamState

----------------------------------------------------------------

newStreamTable :: IO StreamTable
newStreamTable = StreamTable <$> newIORef M.empty

insert :: StreamTable -> M.Key -> Stream -> IO ()
insert (StreamTable ref) k v = atomicModifyIORef' ref $ \m ->
    let m' = M.insert k v m
    in (m', ())

remove :: StreamTable -> M.Key -> IO ()
remove (StreamTable ref) k = atomicModifyIORef' ref $ \m ->
    let m' = M.delete k m
    in (m', ())

search :: StreamTable -> M.Key -> IO (Maybe Stream)
search (StreamTable ref) k = M.lookup k <$> readIORef ref

updateAllStreamWindow :: (WindowSize -> WindowSize) -> StreamTable -> IO ()
updateAllStreamWindow adst (StreamTable ref) = do
    strms <- M.elems <$> readIORef ref
    forM_ strms $ \strm -> atomically $ modifyTVar (streamWindow strm) adst

closeAllStreams :: StreamTable -> Maybe SomeException -> IO ()
closeAllStreams (StreamTable ref) mErr' = do
    strms <- atomicModifyIORef' ref $ \m -> (M.empty, m)
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
             Just err | Just ConnectionIsClosed <- fromException err ->
               Nothing
             _otherwise ->
               mErr'
