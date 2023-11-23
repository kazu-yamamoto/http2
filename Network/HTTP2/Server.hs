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

    -- * HTTP\/2 server
    Server,

    -- * Request
    Request,

    -- ** Accessing request
    requestMethod,
    requestPath,
    requestAuthority,
    requestScheme,
    requestHeaders,
    requestBodySize,
    getRequestBodyChunk,
    getRequestTrailers,

    -- * Aux
    Aux,
    auxTimeHandle,
    auxMySockAddr,
    auxPeerSockAddr,

    -- * Response
    Response,

    -- ** Creating response
    responseNoBody,
    responseFile,
    responseStreaming,
    responseBuilder,

    -- ** Accessing response
    responseBodySize,

    -- ** Trailers maker
    TrailersMaker,
    NextTrailersMaker (..),
    defaultTrailersMaker,
    setResponseTrailersMaker,

    -- * Push promise
    PushPromise,
    pushPromise,
    promiseRequestPath,
    promiseResponse,

    -- * Types
    Path,
    Authority,
    Scheme,
    FileSpec (..),
    FileOffset,
    ByteCount,

    -- * RecvN
    defaultReadN,

    -- * Position read for files
    PositionReadMaker,
    PositionRead,
    Sentinel (..),
    defaultPositionReadMaker,
) where

import Data.ByteString.Builder (Builder)
import Data.IORef (readIORef)
import qualified Network.HTTP.Types as H

import Imports
import Network.HPACK
import Network.HPACK.Token
import Network.HTTP2.Frame.Types
import Network.HTTP2.H2
import Network.HTTP2.Server.Run (
    ServerConfig (..),
    defaultServerConfig,
    run,
 )
import Network.HTTP2.Server.Types

----------------------------------------------------------------

-- | Getting the method from a request.
requestMethod :: Request -> Maybe H.Method
requestMethod (Request req) = getHeaderValue tokenMethod vt
  where
    (_, vt) = inpObjHeaders req

-- | Getting the path from a request.
requestPath :: Request -> Maybe Path
requestPath (Request req) = getHeaderValue tokenPath vt
  where
    (_, vt) = inpObjHeaders req

-- | Getting the authority from a request.
requestAuthority :: Request -> Maybe Authority
requestAuthority (Request req) = getHeaderValue tokenAuthority vt
  where
    (_, vt) = inpObjHeaders req

-- | Getting the scheme from a request.
requestScheme :: Request -> Maybe Scheme
requestScheme (Request req) = getHeaderValue tokenScheme vt
  where
    (_, vt) = inpObjHeaders req

-- | Getting the headers from a request.
requestHeaders :: Request -> HeaderTable
requestHeaders (Request req) = inpObjHeaders req

-- | Getting the body size from a request.
requestBodySize :: Request -> Maybe Int
requestBodySize (Request req) = inpObjBodySize req

-- | Reading a chunk of the request body.
--   An empty 'ByteString' returned when finished.
getRequestBodyChunk :: Request -> IO ByteString
getRequestBodyChunk (Request req) = inpObjBody req

-- | Reading request trailers.
--   This function must be called after 'getRequestBodyChunk'
--   returns an empty.
getRequestTrailers :: Request -> IO (Maybe HeaderTable)
getRequestTrailers (Request req) = readIORef (inpObjTrailers req)

----------------------------------------------------------------

-- | Creating response without body.
responseNoBody :: H.Status -> H.ResponseHeaders -> Response
responseNoBody st hdr = Response $ OutObj hdr' OutBodyNone defaultTrailersMaker
  where
    hdr' = setStatus st hdr

-- | Creating response with file.
responseFile :: H.Status -> H.ResponseHeaders -> FileSpec -> Response
responseFile st hdr fileSpec = Response $ OutObj hdr' (OutBodyFile fileSpec) defaultTrailersMaker
  where
    hdr' = setStatus st hdr

-- | Creating response with builder.
responseBuilder :: H.Status -> H.ResponseHeaders -> Builder -> Response
responseBuilder st hdr builder = Response $ OutObj hdr' (OutBodyBuilder builder) defaultTrailersMaker
  where
    hdr' = setStatus st hdr

-- | Creating response with streaming.
responseStreaming
    :: H.Status
    -> H.ResponseHeaders
    -> ((Builder -> IO ()) -> IO () -> IO ())
    -> Response
responseStreaming st hdr strmbdy = Response $ OutObj hdr' (OutBodyStreaming strmbdy) defaultTrailersMaker
  where
    hdr' = setStatus st hdr

----------------------------------------------------------------

-- | Getter for response body size. This value is available for file body.
responseBodySize :: Response -> Maybe Int
responseBodySize (Response (OutObj _ (OutBodyFile (FileSpec _ _ len)) _)) = Just (fromIntegral len)
responseBodySize _ = Nothing

-- | Setting 'TrailersMaker' to 'Response'.
setResponseTrailersMaker :: Response -> TrailersMaker -> Response
setResponseTrailersMaker (Response rsp) tm = Response rsp{outObjTrailers = tm}

----------------------------------------------------------------

-- | Creating push promise.
--   The third argument is traditional, not used.
pushPromise :: ByteString -> Response -> Weight -> PushPromise
pushPromise path rsp _ = PushPromise path rsp
