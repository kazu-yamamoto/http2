{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
-- > main = runTCPServer Nothing "80" $ \s _peer -> runHTTP2Server s
-- >   where
-- >     runHTTP2Server s = E.bracket (allocSimpleConfig s 4096)
-- >                                  (\config -> run config server)
-- >                                  freeSimpleConfig
-- >     server _req _aux sendResponse = sendResponse response []
-- >       where
-- >         response = responseBuilder ok200 header body
-- >         header = [("Content-Type", "text/plain")]
-- >         body = byteString "Hello, world!\n"

module Network.HTTP2.Server (
  -- * Runner
    run
  -- * Runner arguments
  , Config(..)
  , allocSimpleConfig
  , freeSimpleConfig
  , makeSimpleConfig
  -- * HTTP/2 server
  , Server
  -- * Request
  , Request
  , requestHeaders
  , requestBodySize
  , getRequestBodyChunk
  , getRequestTrailers
  -- * Aux
  , Aux
  , auxTimeHandle
  -- * Response
  , Response
  -- ** Creating response
  , responseNoBody
  , responseFile
  , responseStreaming
  , responseBuilder
  -- ** Accessing response
  , responseStatus
  , responseBodySize
  -- ** Trailers maker
  , TrailersMaker
  , NextTrailersMaker(..)
  , defaultTrailersMaker
  , setResponseTrailersMaker
  -- * Push promise
  , PushPromise
  , pushPromise
  , promiseRequestPath
  , promiseResponse
  , promiseWeight
  -- * Types
  , FileSpec(..)
  , FileOffset
  , ByteCount
  -- * RecvN
  , defaultReadN
  -- * Position read for files
  , PositionReadMaker
  , PositionRead
  , Sentinel(..)
  , defaultPositionReadMaker
  ) where

import Data.IORef (readIORef)
import Data.ByteString.Builder (Builder)
import qualified Network.HTTP.Types as H

import Imports
import Network.HPACK
import Network.HTTP2.Server.API
import Network.HTTP2.Server.Config
import Network.HTTP2.Server.File (defaultPositionReadMaker)
import Network.HTTP2.Server.ReadN (defaultReadN)
import Network.HTTP2.Server.Run (run)
import Network.HTTP2.Types

----------------------------------------------------------------

-- | Reading a chunk of the request body.
--   An empty 'ByteString' returned when finished.
getRequestBodyChunk :: Request -> IO ByteString
getRequestBodyChunk = requestBody

-- | Reading request trailers.
--   This function must be called after 'getRequestBodyChunk'
--   returns an empty.
getRequestTrailers :: Request -> IO (Maybe HeaderTable)
getRequestTrailers = readIORef . requestTrailers_

-- |Creating response without body.
responseNoBody :: H.Status -> H.ResponseHeaders -> Response
responseNoBody st hdr = Response st hdr RspNoBody defaultTrailersMaker

-- |Creating response with file.
responseFile :: H.Status -> H.ResponseHeaders -> FileSpec -> Response
responseFile st hdr fileSpec = Response st hdr (RspFile fileSpec) defaultTrailersMaker

-- |Creating response with builder.
responseBuilder :: H.Status -> H.ResponseHeaders -> Builder -> Response
responseBuilder st hdr builder = Response st hdr (RspBuilder builder) defaultTrailersMaker
-- |Creating response with streaming.
responseStreaming :: H.Status -> H.ResponseHeaders
                  -> ((Builder -> IO ()) -> IO () -> IO ())
                  -> Response
responseStreaming st hdr strmbdy = Response st hdr (RspStreaming strmbdy) defaultTrailersMaker

-- | Getter for response body size. This value is available for file body.
responseBodySize :: Response -> Maybe Int
responseBodySize (Response _ _ (RspFile (FileSpec _ _ len)) _) = Just (fromIntegral len)
responseBodySize _                                             = Nothing

-- | Setting 'TrailersMaker' to 'Response'.
setResponseTrailersMaker :: Response -> TrailersMaker -> Response
setResponseTrailersMaker rsp tm = rsp { responseTrailers = tm }

-- | TrailersMake to create no trailers.
defaultTrailersMaker :: TrailersMaker
defaultTrailersMaker Nothing = return $ Trailers []
defaultTrailersMaker _       = return $ NextTrailersMaker defaultTrailersMaker

-- | Creating push promise.
pushPromise :: ByteString -> Response -> Weight -> PushPromise
pushPromise path rsp w = PushPromise path rsp w
