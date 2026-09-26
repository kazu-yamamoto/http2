{-# LANGUAGE NamedFieldPuns #-}

module Network.HTTP2.H2.Stream where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Data.IORef
import Data.Maybe (fromMaybe)
import Network.Control
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
        <*> newIORef Nothing

newEvenStream :: StreamId -> WindowSize -> WindowSize -> IO Stream
newEvenStream sid txwin rxwin =
    Stream sid
        <$> newTVarIO Reserved
        <*> newEmptyMVar
        <*> newTVarIO (newTxFlow txwin)
        <*> newIORef (newRxFlow rxwin)
        <*> newIORef Nothing
        <*> newIORef Nothing

----------------------------------------------------------------

{-# INLINE readStreamState #-}
readStreamState :: Stream -> IO StreamState
readStreamState Stream{streamState} = readTVarIO streamState

----------------------------------------------------------------

closeAllStreams
    :: TVar OddStreamTable -> TVar EvenStreamTable -> Maybe E.SomeException -> IO ()
closeAllStreams ovar evar mErr = do
    ostrms <- clearOddStreamTable ovar
    mapM_ finalize ostrms
    estrms <- clearEvenStreamTable evar
    mapM_ finalize estrms
  where
    -- We treat /every/ exception, including 'ConectionIsClosed', as abnormal
    -- termination: we should only report a clean termination when we receive an
    -- explicit @END_STREAM@ frame.
    finalize strm = do
        st <- readStreamState strm
        void $ tryPutMVar (streamInput strm) err
        case st of
            Open _ (Body q _ _ _) ->
                atomically $ writeTQueue q $ maybe (Right (mempty, True)) Left mErr
            _otherwise ->
                return ()

    err :: Either E.SomeException a
    err = Left $ fromMaybe (E.toException ConnectionIsClosed) mErr

----------------------------------------------------------------

nextForStreaming
    :: TBQueue StreamingChunk
    -> DynaNext
nextForStreaming tbq =
    let takeQ = atomically $ tryReadTBQueue tbq
        next = fillStreamBodyGetNext takeQ
     in next
