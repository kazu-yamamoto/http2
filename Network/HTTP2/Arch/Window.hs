{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Arch.Window where

import Data.IORef
import qualified UnliftIO.Exception as E
import UnliftIO.STM

import Imports
import Network.HTTP2.Arch.Context
import Network.HTTP2.Arch.EncodeFrame
import Network.HTTP2.Arch.Queue
import Network.HTTP2.Arch.Types
import Network.HTTP2.Frame

getStreamWindowSize :: Stream -> IO WindowSize
getStreamWindowSize Stream{streamWindow} = readTVarIO streamWindow

getConnectionWindowSize :: Context -> IO WindowSize
getConnectionWindowSize Context{txConnectionWindow} = readTVarIO txConnectionWindow

waitStreamWindowSize :: Stream -> IO ()
waitStreamWindowSize Stream{streamWindow} = atomically $ do
    w <- readTVar streamWindow
    checkSTM (w > 0)

waitConnectionWindowSize :: Context -> STM ()
waitConnectionWindowSize Context{txConnectionWindow} = do
    w <- readTVar txConnectionWindow
    checkSTM (w > 0)

----------------------------------------------------------------
-- Receiving window update

increaseWindowSize :: StreamId -> TVar WindowSize -> WindowSize -> IO ()
increaseWindowSize sid tvar n = do
    w <- atomically $ do
        w0 <- readTVar tvar
        let w1 = w0 + n
        writeTVar tvar w1
        return w1
    when (isWindowOverflow w) $ do
        let msg = fromString ("window update for stream " ++ show sid ++ " is overflow")
            err =
                if isControl sid
                    then ConnectionErrorIsSent
                    else StreamErrorIsSent
        E.throwIO $ err FlowControlError sid msg

increaseStreamWindowSize :: Stream -> WindowSize -> IO ()
increaseStreamWindowSize Stream{streamNumber, streamWindow} n =
    increaseWindowSize streamNumber streamWindow n

increaseConnectionWindowSize :: Context -> Int -> IO ()
increaseConnectionWindowSize Context{txConnectionWindow} n =
    increaseWindowSize 0 txConnectionWindow n

decreaseWindowSize :: Context -> Stream -> WindowSize -> IO ()
decreaseWindowSize Context{txConnectionWindow} Stream{streamWindow} siz = do
    atomically $ modifyTVar' txConnectionWindow (subtract siz)
    atomically $ modifyTVar' streamWindow (subtract siz)

----------------------------------------------------------------
-- Sending window update

informWindowUpdate :: Context -> Stream -> IORef Int -> Int -> IO ()
informWindowUpdate _ _ _ 0 = return ()
informWindowUpdate Context{controlQ, rxConnectionInc} Stream{streamNumber} streamInc len = do
    join $ atomicModifyIORef rxConnectionInc $ modify 0
    join $ atomicModifyIORef streamInc $ modify streamNumber
  where
    modify sid w0
        | w1 < thresh = (w1, return ())
        | otherwise =
            let frame = windowUpdateFrame sid w1
                cframe = CFrames Nothing [frame]
                action = enqueueControl controlQ cframe
             in (0, action)
      where
        thresh = defaultWindowSize -- fixme
        w1 = w0 + len
