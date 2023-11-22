{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Settings where

import Data.IORef
import Data.IntMap.Strict (IntMap)

import Imports
import Network.HTTP2.Frame
import Network.HTTP2.H2.Context
import Network.HTTP2.H2.EncodeFrame
import Network.HTTP2.H2.StreamTable
import Network.HTTP2.H2.Types
import Network.HTTP2.H2.Window

-- | Updating settings.
--
-- >>> updateSettings defaultSettings [(SettingsEnablePush,0),(SettingsMaxHeaderBlockSize,200)]
-- Settings {headerTableSize = 4096, enablePush = False, maxConcurrentStreams = Nothing, initialWindowSize = 65535, maxFrameSize = 16384, maxHeaderListSize = Just 200}
{- FOURMOLU_DISABLE -}
updateSettings :: Settings -> SettingsList -> Settings
updateSettings settings kvs = foldl' update settings kvs
  where
    update def (SettingsHeaderTableSize,x)      = def { headerTableSize = x }
    -- fixme: x should be 0 or 1
    update def (SettingsEnablePush,x)           = def { enablePush = x > 0 }
    update def (SettingsMaxConcurrentStreams,x) = def { maxConcurrentStreams = Just x }
    update def (SettingsInitialWindowSize,x)    = def { initialWindowSize = x }
    update def (SettingsMaxFrameSize,x)         = def { maxFrameSize = x }
    update def (SettingsMaxHeaderBlockSize,x)   = def { maxHeaderListSize = Just x }
    update def _                                = def
{- FOURMOLU_ENABLE -}

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
updatePeerSettings Context{peerSettings, oddStreamTable, evenStreamTable} peerAlist = do
    oldws <- initialWindowSize <$> readIORef peerSettings
    modifyIORef' peerSettings $ \old -> updateSettings old peerAlist
    newws <- initialWindowSize <$> readIORef peerSettings
    -- FIXME: race condition
    -- 1) newOddStream reads old peerSettings and
    --    insert it to its stream table after adjusting.
    -- 2) newOddStream reads new peerSettings and
    --    insert it to its stream table before adjusting.
    let diff = newws - oldws
    when (diff /= 0) $ do
        getOddStreams oddStreamTable >>= updateAllStreamTxFlow diff
        getEvenStreams evenStreamTable >>= updateAllStreamTxFlow diff
  where
    updateAllStreamTxFlow :: WindowSize -> IntMap Stream -> IO ()
    updateAllStreamTxFlow siz strms =
        forM_ strms $ \strm -> increaseStreamWindowSize strm siz
