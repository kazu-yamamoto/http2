{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | HTTP\/2 client library.
--
--  Example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
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
-- >     cliconf host = ClientConfig "http" (C8.pack host) 20
-- >     runHTTP2Client host s = E.bracket (allocSimpleConfig s 4096)
-- >                                       freeSimpleConfig
-- >                                       (\conf -> run (cliconf host) conf client)
-- >     client sendRequest = do
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
    ClientConfig (..),
    defaultClientConfig,

    -- * Common configuration
    Config (..),
    allocSimpleConfig,
    freeSimpleConfig,

    -- * HTTP\/2 client
    Client,

    -- * Request
    Request,

    -- * Creating request
    requestNoBody,
    requestFile,
    requestStreaming,
    requestStreamingUnmask,
    requestBuilder,

    -- ** Trailers maker
    TrailersMaker,
    NextTrailersMaker (..),
    defaultTrailersMaker,
    setRequestTrailersMaker,

    -- * Response
    Response,

    -- ** Accessing response
    responseStatus,
    responseHeaders,
    responseBodySize,
    getResponseBodyChunk,
    getResponseTrailers,

    -- * Types
    Method,
    Path,
    FileSpec (..),
    FileOffset,
    ByteCount,

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

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.IORef (readIORef)
import Network.HTTP.Types

import Network.HPACK
import Network.HTTP2.H2
import Network.HTTP2.Client.Run
import Network.HTTP2.Client.Types
import Network.HTTP2.Frame

----------------------------------------------------------------

-- | Creating request without body.
requestNoBody :: Method -> Path -> RequestHeaders -> Request
requestNoBody m p hdr = Request $ OutObj hdr' OutBodyNone defaultTrailersMaker
  where
    hdr' = addHeaders m p hdr

-- | Creating request with file.
requestFile :: Method -> Path -> RequestHeaders -> FileSpec -> Request
requestFile m p hdr fileSpec = Request $ OutObj hdr' (OutBodyFile fileSpec) defaultTrailersMaker
  where
    hdr' = addHeaders m p hdr

-- | Creating request with builder.
requestBuilder :: Method -> Path -> RequestHeaders -> Builder -> Request
requestBuilder m p hdr builder = Request $ OutObj hdr' (OutBodyBuilder builder) defaultTrailersMaker
  where
    hdr' = addHeaders m p hdr

-- | Creating request with streaming.
requestStreaming
    :: Method
    -> Path
    -> RequestHeaders
    -> ((Builder -> IO ()) -> IO () -> IO ())
    -> Request
requestStreaming m p hdr strmbdy = Request $ OutObj hdr' (OutBodyStreaming strmbdy) defaultTrailersMaker
  where
    hdr' = addHeaders m p hdr

-- | Like 'requestStreaming', but run the action with exceptions masked
requestStreamingUnmask
    :: Method
    -> Path
    -> RequestHeaders
    -> ((forall x. IO x -> IO x) -> (Builder -> IO ()) -> IO () -> IO ())
    -> Request
requestStreamingUnmask m p hdr strmbdy = Request $ OutObj hdr' (OutBodyStreamingUnmask strmbdy) defaultTrailersMaker
  where
    hdr' = addHeaders m p hdr

addHeaders :: Method -> Path -> RequestHeaders -> RequestHeaders
addHeaders m p hdr = (":method", m) : (":path", p) : hdr

-- | Setting 'TrailersMaker' to 'Response'.
setRequestTrailersMaker :: Request -> TrailersMaker -> Request
setRequestTrailersMaker (Request req) tm = Request req{outObjTrailers = tm}

----------------------------------------------------------------

-- | Getting the status of a response.
responseStatus :: Response -> Maybe Status
responseStatus (Response rsp) = getStatus $ inpObjHeaders rsp

-- | Getting the headers from a response.
responseHeaders :: Response -> HeaderTable
responseHeaders (Response rsp) = inpObjHeaders rsp

-- | Getting the body size from a response.
responseBodySize :: Response -> Maybe Int
responseBodySize (Response rsp) = inpObjBodySize rsp

-- | Reading a chunk of the response body.
--   An empty 'ByteString' returned when finished.
getResponseBodyChunk :: Response -> IO ByteString
getResponseBodyChunk (Response rsp) = inpObjBody rsp

-- | Reading response trailers.
--   This function must be called after 'getResponseBodyChunk'
--   returns an empty.
getResponseTrailers :: Response -> IO (Maybe HeaderTable)
getResponseTrailers (Response rsp) = readIORef (inpObjTrailers rsp)
