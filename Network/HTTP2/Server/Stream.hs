{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.HTTP2.Server.Stream where

import Control.Concurrent.STM
import Data.IORef
import qualified Data.IntMap.Strict as M

import Imports
import Network.HTTP2
import Network.HTTP2.Priority
import Network.HTTP2.Server.Types

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
isHalfClosedLocal (HalfClosedLocal _) = True
isHalfClosedLocal (Closed _)       = True
isHalfClosedLocal _                = False

isClosed :: StreamState -> Bool
isClosed Closed{} = True
isClosed _        = False

----------------------------------------------------------------

newStream :: StreamId -> WindowSize -> IO Stream
newStream sid win = Stream sid <$> newIORef Idle
                               <*> newTVarIO win
                               <*> newIORef defaultPrecedence

----------------------------------------------------------------

{-# INLINE readStreamState #-}
readStreamState :: Stream -> IO StreamState
readStreamState Stream{streamState} = readIORef streamState

----------------------------------------------------------------

newStreamTable :: IO StreamTable
newStreamTable = StreamTable <$> newIORef M.empty

insert :: StreamTable -> M.Key -> Stream -> IO ()
insert (StreamTable ref) k v = atomicModifyIORef' ref $ \m ->
    let !m' = M.insert k v m
    in (m', ())

remove :: StreamTable -> M.Key -> IO ()
remove (StreamTable ref) k = atomicModifyIORef' ref $ \m ->
    let !m' = M.delete k m
    in (m', ())

search :: StreamTable -> M.Key -> IO (Maybe Stream)
search (StreamTable ref) k = M.lookup k <$> readIORef ref

updateAllStreamWindow :: (WindowSize -> WindowSize) -> StreamTable -> IO ()
updateAllStreamWindow adst (StreamTable ref) = do
    strms <- M.elems <$> readIORef ref
    forM_ strms $ \strm -> atomically $ modifyTVar (streamWindow strm) adst

