{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Arch.Window where

import Data.IORef
import UnliftIO.STM

import Imports
import Network.HTTP2.Arch.Config
import Network.HTTP2.Arch.Context
import Network.HTTP2.Arch.EncodeFrame
import Network.HTTP2.Arch.Queue
import Network.HTTP2.Arch.Stream
import Network.HTTP2.Arch.Types
import Network.HTTP2.Frame

----------------------------------------------------------------
-- Receiving window update

increaseStreamWindowSize :: Stream -> Int -> IO WindowSize
increaseStreamWindowSize Stream{streamWindow} n = atomically $ do
    w0 <- readTVar streamWindow
    let w1 = w0 + n
    writeTVar streamWindow w1
    return w1

increaseConnectionWindowSize :: Context -> Int -> IO WindowSize
increaseConnectionWindowSize Context{txConnectionWindow} n = atomically $ do
    w0 <- readTVar txConnectionWindow
    let w1 = w0 + n
    writeTVar txConnectionWindow w1
    return w1

----------------------------------------------------------------
-- Sending window update

decreaseWindowSize :: Context -> Stream -> WindowSize -> IO ()
decreaseWindowSize Context{txConnectionWindow} Stream{streamWindow} siz = do
    atomically $ modifyTVar' txConnectionWindow (subtract siz)
    atomically $ modifyTVar' streamWindow (subtract siz)

informWindowUpdate :: TQueue Control -> StreamId -> IORef Int -> Int -> IO ()
informWindowUpdate _        _   _      0   = return ()
informWindowUpdate controlQ sid incref len = do
    -- incref is occupied by the receiver thread
    w0 <- readIORef incref
    let w1 = w0 + len
    if w1 >= defaultWindowSize then do -- fixme
        let frame = windowUpdateFrame sid w1
        enqueueControl controlQ $ CFrames Nothing [frame]
        writeIORef incref 0
      else
        writeIORef incref w1

informConnectionWindowUpdate :: Context -> Int -> IO ()
informConnectionWindowUpdate Context{..} =
    informWindowUpdate controlQ 0 rxConnectionInc

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

updateMySettings :: Config -> Context -> IO [ByteString]
updateMySettings Config{..} Context{myFirstSettings,myPendingAlist} = do
    writeIORef myFirstSettings True
    writeIORef myPendingAlist $ Just myInitialAlist
    return frames
  where
    len = confBufferSize - frameHeaderLength
    payloadLen = max defaultPayloadLength len
    myInitialAlist =
        -- confBufferSize is the size of the write buffer.
        -- But we assume that the size of the read buffer is the same size.
        -- So, the size is announced to via SETTINGS_MAX_FRAME_SIZE.
        [(SettingsMaxFrameSize,payloadLen)
        ,(SettingsMaxConcurrentStreams,recommendedConcurrency)
        -- Initial window size for streams
        ,(SettingsInitialWindowSize,maxWindowSize)]
    frame1 = settingsFrame id myInitialAlist
        -- Initial window update for connection
    frame2 = windowUpdateFrame 0 (maxWindowSize - defaultWindowSize)
    frames = [frame1,frame2]

-- Peer SETTINGS_INITIAL_WINDOW_SIZE
-- Adjusting initial window size for streams
updatePeerSettings :: Context -> SettingsList -> IO ()
updatePeerSettings Context{peerSettings,streamTable} peerAlist = do
    oldws <- initialWindowSize <$> readIORef peerSettings
    modifyIORef' peerSettings $ \old -> updateSettings old peerAlist
    newws <- initialWindowSize <$> readIORef peerSettings
    let diff = newws - oldws
    when (diff /= 0) $ updateAllStreamWindow (+ diff) streamTable
