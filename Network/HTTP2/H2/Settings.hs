module Network.HTTP2.H2.Settings where

import Network.Control

import Imports
import Network.HTTP2.Frame
import Network.HTTP2.H2.EncodeFrame

----------------------------------------------------------------

-- | HTTP\/2 settings. See <https://datatracker.ietf.org/doc/html/rfc9113#name-defined-settings>.
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
    , pingRateLimit :: Int
    -- ^ Maximum number of pings allowed per second (CVE-2019-9512)
    , emptyFrameRateLimit :: Int
    -- ^ Maximum number of empty data frames allowed per second (CVE-2019-9518)
    , settingsRateLimit :: Int
    -- ^ Maximum number of settings frames allowed per second (CVE-2019-9515)
    , rstRateLimit :: Int
    -- ^ Maximum number of reset frames allowed per second (CVE-2023-44487)
    }
    deriving (Eq, Show)

-- | The default settings.
--
-- >>> baseSettings
-- Settings {headerTableSize = 4096, enablePush = True, maxConcurrentStreams = Nothing, initialWindowSize = 65535, maxFrameSize = 16384, maxHeaderListSize = Nothing, pingRateLimit = 10, emptyFrameRateLimit = 4, settingsRateLimit = 4, rstRateLimit = 4}
baseSettings :: Settings
baseSettings =
    Settings
        { headerTableSize = 4096 -- defaultDynamicTableSize
        , enablePush = True
        , maxConcurrentStreams = Nothing
        , initialWindowSize = defaultWindowSize -- 64K (65,535)
        , maxFrameSize = defaultPayloadLength -- 2^14 (16,384)
        , maxHeaderListSize = Nothing
        , pingRateLimit = 10
        , emptyFrameRateLimit = 4
        , settingsRateLimit = 4
        , rstRateLimit = 4
        }

-- | The default settings.
--
-- >>> defaultSettings
-- Settings {headerTableSize = 4096, enablePush = True, maxConcurrentStreams = Just 64, initialWindowSize = 262144, maxFrameSize = 16384, maxHeaderListSize = Nothing, pingRateLimit = 10, emptyFrameRateLimit = 4, settingsRateLimit = 4, rstRateLimit = 4}
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
-- Settings {headerTableSize = 4096, enablePush = False, maxConcurrentStreams = Just 64, initialWindowSize = 262144, maxFrameSize = 16384, maxHeaderListSize = Just 200, pingRateLimit = 10, emptyFrameRateLimit = 4, settingsRateLimit = 4, rstRateLimit = 4}
{- FOURMOLU_DISABLE -}
fromSettingsList :: Settings -> SettingsList -> Settings
fromSettingsList settings kvs = foldl' update settings kvs
  where
    update def (SettingsTokenHeaderTableSize,x)      = def { headerTableSize = x }
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
            SettingsTokenHeaderTableSize
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
    alist = toSettingsList settings baseSettings
    frame1 = settingsFrame id alist
    frames =
        if connWindowSize /= defaultWindowSize
            then [windowUpdateFrame 0 (connWindowSize - defaultWindowSize)]
            else []

----------------------------------------------------------------
