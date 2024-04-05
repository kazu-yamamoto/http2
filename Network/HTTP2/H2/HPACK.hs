{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.HPACK (
    hpackEncodeHeader,
    hpackEncodeHeaderLoop,
    hpackDecodeHeader,
    hpackDecodeTrailer,
    just,
    fixHeaders,
) where

import qualified Control.Exception as E
import Network.ByteOrder
import qualified Network.HTTP.Types as H

import Imports
import Network.HPACK
import Network.HTTP.Semantics.Token
import Network.HTTP2.Frame
import Network.HTTP2.H2.Context
import Network.HTTP2.H2.Types

-- $setup
-- >>> :set -XOverloadedStrings

----------------------------------------------------------------

fixHeaders :: H.ResponseHeaders -> H.ResponseHeaders
fixHeaders hdr = deleteUnnecessaryHeaders hdr

deleteUnnecessaryHeaders :: H.ResponseHeaders -> H.ResponseHeaders
deleteUnnecessaryHeaders hdr = filter del hdr
  where
    del (k, _) = k `notElem` headersToBeRemoved

headersToBeRemoved :: [H.HeaderName]
headersToBeRemoved =
    [ H.hConnection
    , "Transfer-Encoding"
    -- Keep-Alive
    -- Proxy-Connection
    -- Upgrade
    ]

----------------------------------------------------------------

strategy :: EncodeStrategy
strategy = EncodeStrategy{compressionAlgo = Linear, useHuffman = False}

-- Set-Cookie: contains only one cookie value.
-- So, we don't need to split it.
hpackEncodeHeader
    :: Context
    -> Buffer
    -> BufferSize
    -> TokenHeaderList
    -> IO (TokenHeaderList, Int)
hpackEncodeHeader Context{..} buf siz ths =
    encodeTokenHeader buf siz strategy True encodeDynamicTable ths

hpackEncodeHeaderLoop
    :: Context
    -> Buffer
    -> BufferSize
    -> TokenHeaderList
    -> IO (TokenHeaderList, Int)
hpackEncodeHeaderLoop Context{..} buf siz hs =
    encodeTokenHeader buf siz strategy False encodeDynamicTable hs

----------------------------------------------------------------

hpackDecodeHeader
    :: HeaderBlockFragment -> StreamId -> Context -> IO HeaderTable
hpackDecodeHeader hdrblk sid ctx = do
    tbl@(_, vt) <- hpackDecodeTrailer hdrblk sid ctx
    if isClient ctx || checkRequestHeader vt
        then return tbl
        else E.throwIO $ StreamErrorIsSent ProtocolError sid "illegal header"

hpackDecodeTrailer
    :: HeaderBlockFragment -> StreamId -> Context -> IO HeaderTable
hpackDecodeTrailer hdrblk sid Context{..} = decodeTokenHeader decodeDynamicTable hdrblk `E.catch` handl
  where
    handl IllegalHeaderName =
        E.throwIO $ StreamErrorIsSent ProtocolError sid "illegal trailer"
    handl e = do
        let msg = fromString $ show e
        E.throwIO $ StreamErrorIsSent CompressionError sid msg

{-# INLINE checkRequestHeader #-}
checkRequestHeader :: ValueTable -> Bool
checkRequestHeader reqvt
    | just mMethod (== "CONNECT") = isNothing mPath && isNothing mScheme
    | isJust mStatus = False
    | isNothing mMethod = False
    | isNothing mScheme = False
    | isNothing mPath = False
    | mPath == Just "" = False
    | isJust mConnection = False
    | just mTE (/= "trailers") = False
    | otherwise = checkAuth mAuthority mHost
  where
    mStatus = getHeaderValue tokenStatus reqvt
    mScheme = getHeaderValue tokenScheme reqvt
    mPath = getHeaderValue tokenPath reqvt
    mMethod = getHeaderValue tokenMethod reqvt
    mConnection = getHeaderValue tokenConnection reqvt
    mTE = getHeaderValue tokenTE reqvt
    mAuthority = getHeaderValue tokenAuthority reqvt
    mHost = getHeaderValue tokenHost reqvt

checkAuth :: Maybe ByteString -> Maybe ByteString -> Bool
checkAuth Nothing Nothing = False
checkAuth (Just a) (Just h) | a /= h = False
checkAuth _ _ = True

{-# INLINE just #-}
just :: Maybe a -> (a -> Bool) -> Bool
just Nothing _ = False
just (Just x) p
    | p x = True
    | otherwise = False
