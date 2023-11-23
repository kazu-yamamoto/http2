{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Settings where

import Network.Control

import Imports
import Network.HTTP2.Frame
import Network.HTTP2.H2.EncodeFrame

----------------------------------------------------------------

-- | Cooked version of settings. This is suitable to be stored in a HTTP/2 context.
data Settings = Settings
    { headerTableSize :: Int
    -- ^ SETTINGS_HEADER_TABLE_SIZE
    , enablePush :: Bool
    -- ^ SETTINGS_ENABLE_PUSH
    , maxConcurrentStreams :: Maybe Int
    -- ^ SETTINGS_MAX_CONCURRENT_STREAMS
    , initialWindowSize :: WindowSize
    -- ^ SETTINGS_INITIAL_WINDOW_SIZE
    , maxFrameSize :: Int
    -- ^ SETTINGS_MAX_FRAME_SIZE
    , maxHeaderListSize :: Maybe Int
    -- ^ SETTINGS_MAX_HEADER_LIST_SIZE
    }
    deriving (Eq, Show)

-- | The default settings.
--
-- >>> baseSettings
-- Settings {headerTableSize = 4096, enablePush = True, maxConcurrentStreams = Nothing, initialWindowSize = 65535, maxFrameSize = 16384, maxHeaderListSize = Nothing}
baseSettings :: Settings
baseSettings =
    Settings
        { headerTableSize = 4096 -- defaultDynamicTableSize
        , enablePush = True
        , maxConcurrentStreams = Nothing
        , initialWindowSize = defaultWindowSize
        , maxFrameSize = defaultPayloadLength
        , maxHeaderListSize = Nothing
        }

-- | The default settings.
--
-- >>> defaultSettings
-- Settings {headerTableSize = 4096, enablePush = True, maxConcurrentStreams = Just 64, initialWindowSize = 262144, maxFrameSize = 16384, maxHeaderListSize = Nothing}
defaultSettings :: Settings
defaultSettings =
    baseSettings
        { maxConcurrentStreams = Just defaultMaxStreams
        , initialWindowSize = defaultMaxStreamData
        }

----------------------------------------------------------------

-- | Updating settings.
--
-- >>> fromSettingsList defaultSettings [(SettingsEnablePush,0),(SettingsMaxHeaderListSize,200)]
-- Settings {headerTableSize = 4096, enablePush = False, maxConcurrentStreams = Just 64, initialWindowSize = 262144, maxFrameSize = 16384, maxHeaderListSize = Just 200}
{- FOURMOLU_DISABLE -}
fromSettingsList :: Settings -> SettingsList -> Settings
fromSettingsList settings kvs = foldl' update settings kvs
  where
    update def (SettingsHeaderTableSize,x)      = def { headerTableSize = x }
    -- fixme: x should be 0 or 1
    update def (SettingsEnablePush,x)           = def { enablePush = x > 0 }
    update def (SettingsMaxConcurrentStreams,x) = def { maxConcurrentStreams = Just x }
    update def (SettingsInitialWindowSize,x)    = def { initialWindowSize = x }
    update def (SettingsMaxFrameSize,x)         = def { maxFrameSize = x }
    update def (SettingsMaxHeaderListSize,x)    = def { maxHeaderListSize = Just x }
    update def _                                = def
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

diff
    :: Eq a
    => Settings
    -> Settings
    -> (Settings -> a)
    -> SettingsKey
    -> (a -> SettingsValue)
    -> Maybe (SettingsKey, SettingsValue)
diff settings settings0 label key enc
    | val == val0 = Nothing
    | otherwise = Just (key, enc val)
  where
    val = label settings
    val0 = label settings0

toSettingsList :: Settings -> Settings -> SettingsList
toSettingsList s s0 =
    catMaybes
        [ diff
            s
            s0
            headerTableSize
            SettingsHeaderTableSize
            id
        , diff
            s
            s0
            enablePush
            SettingsEnablePush
            (const 0) -- fixme
        , diff
            s
            s0
            maxConcurrentStreams
            SettingsMaxConcurrentStreams
            fromJust
        , diff
            s
            s0
            initialWindowSize
            SettingsInitialWindowSize
            id
        , diff
            s
            s0
            maxFrameSize
            SettingsMaxFrameSize
            id
        , diff
            s
            s0
            maxHeaderListSize
            SettingsMaxHeaderListSize
            fromJust
        ]

----------------------------------------------------------------

makeNegotiationFrames :: Settings -> WindowSize -> [ByteString]
makeNegotiationFrames settings connWindowSize = frame1 : frames
  where
    alist = toSettingsList settings defaultSettings
    frame1 = settingsFrame id alist
    frames =
        if connWindowSize /= defaultWindowSize
            then [windowUpdateFrame 0 (connWindowSize - defaultWindowSize)]
            else []

----------------------------------------------------------------
