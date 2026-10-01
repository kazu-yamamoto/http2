{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2.H2.Window where

import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.IORef
import Network.Control

import Imports
import Network.HTTP2.Frame
import Network.HTTP2.H2.Context
import Network.HTTP2.H2.EncodeFrame
import Network.HTTP2.H2.Queue
import Network.HTTP2.H2.Types

getStreamWindowSize :: Stream -> IO WindowSize
getStreamWindowSize Stream{streamTxFlow} =
    txWindowSize <$> readTVarIO streamTxFlow

getConnectionWindowSize :: Context -> IO WindowSize
getConnectionWindowSize Context{txFlow} =
    txWindowSize <$> readTVarIO txFlow

waitStreamWindowSize :: Stream -> IO ()
waitStreamWindowSize Stream{streamTxFlow} = atomically $ do
    w <- txWindowSize <$> readTVar streamTxFlow
    check (w > 0)

waitConnectionWindowSize :: Context -> STM ()
waitConnectionWindowSize Context{txFlow} = do
    w <- txWindowSize <$> readTVar txFlow
    check (w > 0)

----------------------------------------------------------------
-- Receiving window update

increaseWindowSize :: StreamId -> TVar TxFlow -> WindowSize -> IO ()
increaseWindowSize sid tvar n = do
    atomically $ modifyTVar' tvar $ \flow -> flow{txfLimit = txfLimit flow + n}
    w <- txWindowSize <$> readTVarIO tvar
    when (isWindowOverflow w) $ do
        let msg = fromString ("window update for stream " ++ show sid ++ " is overflow")
            err =
                if isControl sid
                    then ConnectionErrorIsSent
                    else StreamErrorIsSent
        E.throwIO $ err FlowControlError sid msg

increaseStreamWindowSize :: Stream -> WindowSize -> IO ()
increaseStreamWindowSize Stream{streamNumber, streamTxFlow} n =
    increaseWindowSize streamNumber streamTxFlow n

increaseConnectionWindowSize :: Context -> Int -> IO ()
increaseConnectionWindowSize Context{txFlow} n =
    increaseWindowSize 0 txFlow n

decreaseWindowSize :: Context -> Stream -> WindowSize -> IO ()
decreaseWindowSize Context{txFlow} Stream{streamTxFlow} siz = do
    dec txFlow
    dec streamTxFlow
  where
    dec tvar = atomically $ modifyTVar' tvar $ \flow -> flow{txfSent = txfSent flow + siz}

----------------------------------------------------------------
-- Sending window update

informWindowUpdate :: Context -> Stream -> Int -> IO ()
informWindowUpdate _ _ 0 = return ()
informWindowUpdate Context{controlQ, rxFlow} Stream{streamNumber, streamRxFlow} len = do
    mxc <- atomicModifyIORef rxFlow $ maybeOpenRxWindow len FCTWindowUpdate
    forM_ mxc $ \ws -> do
        let frame = windowUpdateFrame 0 ws
            cframe = CFrames Nothing [frame]
        enqueueControl controlQ cframe
    mxs <- atomicModifyIORef streamRxFlow $ maybeOpenRxWindow len FCTWindowUpdate
    forM_ mxs $ \ws -> do
        let frame = windowUpdateFrame streamNumber ws
            cframe = CFrames Nothing [frame]
        enqueueControl controlQ cframe

-- | Account for a DATA frame that is being dropped.
--
-- Its stream is gone -- reset, or closed and forgotten -- so there is no
-- stream window to adjust.  The peer charged these octets against the
-- connection window before sending them, though, and if we say nothing its
-- view of that window shrinks for good; enough dropped frames and the
-- connection stalls with both sides believing the other is at fault.  So
-- charge them and give them straight back.
informIgnoredData :: Context -> StreamId -> Int -> IO ()
informIgnoredData _ _ 0 = return ()
informIgnoredData ctx@Context{rxFlow} sid len = do
    ok <- atomicModifyIORef' rxFlow $ checkRxLimit len
    unless ok $
        E.throwIO $
            ConnectionErrorIsSent
                EnhanceYourCalm
                sid
                "exceeds connection flow-control limit"
    giveBackConnectionWindow ctx len

-- | Give octets already charged to the connection window back to it, and to
-- it alone: for a stream that is closed, whose own window no longer
-- matters.
giveBackConnectionWindow :: Context -> Int -> IO ()
giveBackConnectionWindow _ 0 = return ()
giveBackConnectionWindow Context{controlQ, rxFlow} len = do
    mxc <- atomicModifyIORef rxFlow $ maybeOpenRxWindow len FCTWindowUpdate
    forM_ mxc $ \ws ->
        enqueueControl controlQ $ CFrames Nothing [windowUpdateFrame 0 ws]

-- This must be called after an application is finished
-- to adjust RX window.
adjustRxWindow :: Context -> Stream -> IO ()
adjustRxWindow ctx stream = do
    len <- takeUnread stream
    informWindowUpdate ctx stream len

-- | Like 'adjustRxWindow', for a stream that has been closed: what was
-- left unread goes back to the connection window only.
--
-- Closed first, so that nothing is queued after this has looked: the
-- receiver does not queue DATA for a closed stream ('stream'), but gives
-- it back to the connection window itself.
giveBackUnread :: Context -> Stream -> IO ()
giveBackUnread ctx stream = takeUnread stream >>= giveBackConnectionWindow ctx

-- | Take what is left unread in a stream's queue, and say how many octets
-- of body it was.
takeUnread :: Stream -> IO Int
takeUnread Stream{streamRxQ} = do
    mq <- readIORef streamRxQ
    case mq of
        Nothing -> return 0
        Just q -> atomically $ loop q 0
  where
    loop q !total = do
        meb <- tryReadTQueue q
        case meb of
            Just (Right (bs, _)) -> loop q (total + BS.length bs)
            Just le@(Left _) -> do
                -- reserving HTTP2Error
                writeTQueue q le
                return total
            _ -> return total
