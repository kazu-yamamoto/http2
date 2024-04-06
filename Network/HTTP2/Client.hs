{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | HTTP\/2 client library.
--
--  Example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE RankNTypes #-}
-- >
-- > module Main where
-- >
-- > import Control.Concurrent.Async
-- > import qualified Control.Exception as E
-- > import qualified Data.ByteString.Char8 as C8
-- > import Network.HTTP.Types
-- > import Network.Run.TCP (runTCPClient) -- network-run
-- >
-- > import Network.HTTP2.Client
-- >
-- > serverName :: String
-- > serverName = "127.0.0.1"
-- >
-- > main :: IO ()
-- > main = runTCPClient serverName "80" $ runHTTP2Client serverName
-- >   where
-- >     cliconf host = defaultClientConfig { authority = C8.pack host }
-- >     runHTTP2Client host s = E.bracket (allocSimpleConfig s 4096)
-- >                                       freeSimpleConfig
-- >                                       (\conf -> run (cliconf host) conf client)
-- >     client :: Client ()
-- >     client sendRequest _aux = do
-- >         let req0 = requestNoBody methodGet "/" []
-- >             client0 = sendRequest req0 $ \rsp -> do
-- >                 print rsp
-- >                 getResponseBodyChunk rsp >>= C8.putStrLn
-- >             req1 = requestNoBody methodGet "/foo" []
-- >             client1 = sendRequest req1 $ \rsp -> do
-- >                 print rsp
-- >                 getResponseBodyChunk rsp >>= C8.putStrLn
-- >         ex <- E.try $ concurrently_ client0 client1
-- >         case ex of
-- >           Left  e  -> print (e :: HTTP2Error)
-- >           Right () -> putStrLn "OK"
module Network.HTTP2.Client (
    -- * Runner
    run,

    -- * Client configuration
    Scheme,
    Authority,
    ClientConfig,
    defaultClientConfig,
    scheme,
    authority,
    cacheLimit,
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
    pingRateLimit,

    -- * Common configuration
    Config (..),
    allocSimpleConfig,
    freeSimpleConfig,
    module Network.HTTP.Semantics.Client,

    -- * Error
    HTTP2Error (..),
    ReasonPhrase,
    ErrorCode (
        ErrorCode,
        NoError,
        ProtocolError,
        InternalError,
        FlowControlError,
        SettingsTimeout,
        StreamClosed,
        FrameSizeError,
        RefusedStream,
        Cancel,
        CompressionError,
        ConnectError,
        EnhanceYourCalm,
        InadequateSecurity,
        HTTP11Required
    ),

    -- * RecvN
    defaultReadN,

    -- * Position read for files
    PositionReadMaker,
    PositionRead,
    Sentinel (..),
    defaultPositionReadMaker,
) where

import Network.HTTP.Semantics.Client

import Imports
import Network.HTTP2.Client.Run
import Network.HTTP2.Frame
