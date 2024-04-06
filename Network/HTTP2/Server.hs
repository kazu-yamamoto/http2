{-# LANGUAGE OverloadedStrings #-}

-- | HTTP\/2 server library.
--
--  Example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main (main) where
-- >
-- > import qualified Control.Exception as E
-- > import Data.ByteString.Builder (byteString)
-- > import Network.HTTP.Types (ok200)
-- > import Network.Run.TCP (runTCPServer) -- network-run
-- >
-- > import Network.HTTP2.Server
-- >
-- > main :: IO ()
-- > main = runTCPServer Nothing "80" runHTTP2Server
-- >   where
-- >     runHTTP2Server s = E.bracket (allocSimpleConfig s 4096)
-- >                                  freeSimpleConfig
-- >                                  (\config -> run defaultServerConfig config server)
-- >     server _req _aux sendResponse = sendResponse response []
-- >       where
-- >         response = responseBuilder ok200 header body
-- >         header = [("Content-Type", "text/plain")]
-- >         body = byteString "Hello, world!\n"
module Network.HTTP2.Server (
    -- * Runner
    run,

    -- * Server configuration
    ServerConfig,
    defaultServerConfig,
    numberOfWorkers,
    connectionWindowSize,
    settings,

    -- * HTTP\/2 setting
    Settings,
    defaultSettings,
    headerTableSize,
    enablePush,
    maxConcurrentStreams,
    initialWindowSize,
    maxFrameSize,
    maxHeaderListSize,

    -- * Common configuration
    Config (..),
    allocSimpleConfig,
    freeSimpleConfig,
    module Network.HTTP.Semantics.Server,
) where

import Network.HTTP.Semantics.Server

import Network.HTTP2.Server.Run (
    ServerConfig (..),
    defaultServerConfig,
    run,
 )
