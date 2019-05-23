{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.HTTP2.Server.Context where

import Control.Concurrent.STM
import Data.IORef

import Imports
import Network.HPACK
import Network.HTTP2
import Network.HTTP2.Priority
import Network.HTTP2.Server.Stream
import Network.HTTP2.Server.Types

----------------------------------------------------------------

-- | The context for HTTP/2 connection.
data Context = Context {
  -- HTTP/2 settings received from a browser
    http2settings      :: !(IORef Settings)
  , firstSettings      :: !(IORef Bool)
  , streamTable        :: !StreamTable
  , concurrency        :: !(IORef Int)
  , priorityTreeSize   :: !(IORef Int)
  -- | RFC 7540 says "Other frames (from any stream) MUST NOT
  --   occur between the HEADERS frame and any CONTINUATION
  --   frames that might follow". This field is used to implement
  --   this requirement.
  , continued          :: !(IORef (Maybe StreamId))
  , clientStreamId     :: !(IORef StreamId)
  , serverStreamId     :: !(IORef StreamId)
  , inputQ             :: !(TQueue Input)
  , outputQ            :: !(PriorityTree Output)
  , controlQ           :: !(TQueue Control)
  , encodeDynamicTable :: !DynamicTable
  , decodeDynamicTable :: !DynamicTable
  -- the connection window for data from a server to a browser.
  , connectionWindow   :: !(TVar WindowSize)
  }

----------------------------------------------------------------

newContext :: IO Context
newContext = Context <$> newIORef defaultSettings
                     <*> newIORef False
                     <*> newStreamTable
                     <*> newIORef 0
                     <*> newIORef 0
                     <*> newIORef Nothing
                     <*> newIORef 0
                     <*> newIORef 0
                     <*> newTQueueIO
                     <*> newPriorityTree
                     <*> newTQueueIO
                     <*> newDynamicTableForEncoding defaultDynamicTableSize
                     <*> newDynamicTableForDecoding defaultDynamicTableSize 4096
                     <*> newTVarIO defaultInitialWindowSize

clearContext :: Context -> IO ()
clearContext _ctx = return ()

----------------------------------------------------------------

newPushStream :: Context -> WindowSize -> Precedence -> IO Stream
newPushStream Context{serverStreamId} win pre = do
    sid <- atomicModifyIORef' serverStreamId inc2
    Stream sid <$> newIORef Reserved
               <*> newTVarIO win
               <*> newIORef pre
  where
    inc2 x = let !x' = x + 2 in (x', x')

----------------------------------------------------------------

{-# INLINE setStreamState #-}
setStreamState :: Context -> Stream -> StreamState -> IO ()
setStreamState _ Stream{streamState} val = writeIORef streamState val

opened :: Context -> Stream -> IO ()
opened ctx@Context{concurrency} strm = do
    atomicModifyIORef' concurrency (\x -> (x+1,()))
    setStreamState ctx strm (Open JustOpened)

halfClosedRemote :: Context -> Stream -> IO ()
halfClosedRemote ctx stream@Stream{streamState} = do
    !closingCode <- atomicModifyIORef streamState closeHalf
    traverse_ (closed ctx stream) closingCode
  where
    closeHalf :: StreamState -> (StreamState, Maybe ClosedCode)
    closeHalf x@(Closed _)         = (x, Nothing)
    closeHalf (HalfClosedLocal cc) = (Closed cc, Just cc)
    closeHalf _                    = (HalfClosedRemote, Nothing)

halfClosedLocal :: Context -> Stream -> ClosedCode -> IO ()
halfClosedLocal ctx stream@Stream{streamState} cc = do
    shouldFinalize <- atomicModifyIORef streamState closeHalf
    when shouldFinalize $
        closed ctx stream cc
  where
    closeHalf :: StreamState -> (StreamState, Bool)
    closeHalf x@(Closed _)     = (x, False)
    closeHalf HalfClosedRemote = (Closed cc, True)
    closeHalf _                = (HalfClosedLocal cc, False)

closed :: Context -> Stream -> ClosedCode -> IO ()
closed ctx@Context{concurrency,streamTable} strm@Stream{streamNumber} cc = do
    remove streamTable streamNumber
    -- TODO: prevent double-counting
    atomicModifyIORef' concurrency (\x -> (x-1,()))
    setStreamState ctx strm (Closed cc) -- anyway
