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
-- > main = runTCPServer Nothing "80" $ \s _peer -> runHTTP2Server s
-- >   where
-- >     runHTTP2Server s = E.bracket (allocSimpleConfig s 4096)
-- >                                  freeSimpleConfig
-- >                                  (\config -> run config server)
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
  -- * HTTP\/2 server
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

import Data.ByteString.Builder (Builder)
import Data.ByteString.Internal (unsafeCreate)
import Data.IORef (readIORef)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (poke)
import qualified Network.HTTP.Types as H

import Imports
import Network.HPACK
import Network.HTTP2.Arch
import Network.HTTP2.Frame.Types
import Network.HTTP2.Server.Run (run)
import Network.HTTP2.Server.Types

----------------------------------------------------------------

-- | Getting headers from a request.
requestHeaders :: Request -> HeaderTable
requestHeaders = inpObjHeaders

-- | Getting the body size from a request.
requestBodySize :: Request -> Maybe Int
requestBodySize = inpObjBodySize

-- | Reading a chunk of the request body.
--   An empty 'ByteString' returned when finished.
getRequestBodyChunk :: Request -> IO ByteString
getRequestBodyChunk = inpObjBody

-- | Reading request trailers.
--   This function must be called after 'getRequestBodyChunk'
--   returns an empty.
getRequestTrailers :: Request -> IO (Maybe HeaderTable)
getRequestTrailers = readIORef . inpObjTrailers

----------------------------------------------------------------

-- | Creating response without body.
responseNoBody :: H.Status -> H.ResponseHeaders -> Response
responseNoBody st hdr = OutObj hdr' OutBodyNone defaultTrailersMaker
  where
    hdr' = addStatus st hdr

-- | Creating response with file.
responseFile :: H.Status -> H.ResponseHeaders -> FileSpec -> Response
responseFile st hdr fileSpec = OutObj hdr' (OutBodyFile fileSpec) defaultTrailersMaker
  where
    hdr' = addStatus st hdr

-- | Creating response with builder.
responseBuilder :: H.Status -> H.ResponseHeaders -> Builder -> Response
responseBuilder st hdr builder = OutObj hdr' (OutBodyBuilder builder) defaultTrailersMaker
  where
    hdr' = addStatus st hdr

-- | Creating response with streaming.
responseStreaming :: H.Status -> H.ResponseHeaders
                  -> ((Builder -> IO ()) -> IO () -> IO ())
                  -> Response
responseStreaming st hdr strmbdy = OutObj hdr' (OutBodyStreaming strmbdy) defaultTrailersMaker
  where
    hdr' = addStatus st hdr

----------------------------------------------------------------

addStatus :: H.Status -> H.ResponseHeaders -> H.ResponseHeaders
addStatus st hdr = (":status", packStatus st) : hdr

packStatus :: H.Status -> ByteString
packStatus status = unsafeCreate 3 $ \p -> do
    poke p               (toW8 r2)
    poke (p `plusPtr` 1) (toW8 r1)
    poke (p `plusPtr` 2) (toW8 r0)
  where
    toW8 :: Int -> Word8
    toW8 n = 48 + fromIntegral n
    s = fromIntegral $ H.statusCode status
    (q0,r0) = s `divMod` 10
    (q1,r1) = q0 `divMod` 10
    r2 = q1 `mod` 10

----------------------------------------------------------------

-- | Getter for response body size. This value is available for file body.
responseBodySize :: Response -> Maybe Int
responseBodySize (OutObj _ (OutBodyFile (FileSpec _ _ len)) _) = Just (fromIntegral len)
responseBodySize _                                             = Nothing

-- | Setting 'TrailersMaker' to 'Response'.
setResponseTrailersMaker :: Response -> TrailersMaker -> Response
setResponseTrailersMaker rsp tm = rsp { outObjTrailers = tm }

----------------------------------------------------------------

-- | Creating push promise.
pushPromise :: ByteString -> Response -> Weight -> PushPromise
pushPromise path rsp w = PushPromise path rsp w
