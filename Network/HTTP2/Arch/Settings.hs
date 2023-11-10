{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Arch.Settings where

import Data.IORef
import UnliftIO.STM

import Imports
import Network.HTTP2.Arch.Config
import Network.HTTP2.Arch.Context
import Network.HTTP2.Arch.EncodeFrame
import Network.HTTP2.Arch.StreamTable
import Network.HTTP2.Arch.Types
import Network.HTTP2.Frame

-- max: 2,147,483,647 (2^31-1) is too large.
-- def:        65,535 (2^16-1) it too small.
--          1,048,575 (2^20-1)
properWindowSize :: WindowSize
properWindowSize = 1048575

properConcurrentStreams :: Int
properConcurrentStreams = 64

makeMySettingsList :: Config -> Int -> WindowSize -> [(SettingsKey, Int)]
makeMySettingsList Config{..} maxConc winSiz = myInitialAlist
  where
    -- confBufferSize is the size of the write buffer.
    -- But we assume that the size of the read buffer is the same size.
    -- So, the size is announced to via SETTINGS_MAX_FRAME_SIZE.
    len = confBufferSize - frameHeaderLength
    payloadLen = max defaultPayloadLength len
    myInitialAlist =
        [ (SettingsMaxFrameSize, payloadLen)
        , (SettingsMaxConcurrentStreams, maxConc)
        , (SettingsInitialWindowSize, winSiz)
        ]

----------------------------------------------------------------

pendingMySettings :: Context -> IO [ByteString]
pendingMySettings Context{mySettingAlist, myFirstSettings, myPendingAlist} = do
    writeIORef myFirstSettings True
    writeIORef myPendingAlist $ Just mySettingAlist
    return frames'
  where
    frame1 = settingsFrame id mySettingAlist
    -- Initial window update for connection
    frames = case lookup SettingsInitialWindowSize mySettingAlist of
      Nothing -> []
      Just winSiz -> [windowUpdateFrame 0 (winSiz - defaultWindowSize)]
    frames' = frame1 : frames

----------------------------------------------------------------

-- Peer SETTINGS_INITIAL_WINDOW_SIZE
-- Adjusting initial window size for streams
updatePeerSettings :: Context -> SettingsList -> IO ()
updatePeerSettings Context{peerSettings, oddStreamTable} peerAlist = do
    oldws <- initialWindowSize <$> readIORef peerSettings
    modifyIORef' peerSettings $ \old -> updateSettings old peerAlist
    newws <- initialWindowSize <$> readIORef peerSettings
    let diff = newws - oldws
    when (diff /= 0) $ updateAllOddStreamWindow (+ diff) oddStreamTable

----------------------------------------------------------------

updateAllOddStreamWindow
    :: (WindowSize -> WindowSize) -> TVar OddStreamTable -> IO ()
updateAllOddStreamWindow adst var = do
    strms <- getOddStreams var
    forM_ strms $ \strm -> atomically $ modifyTVar (streamWindow strm) adst
