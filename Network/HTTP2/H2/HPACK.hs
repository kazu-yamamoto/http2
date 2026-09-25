{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.HPACK (
    hpackEncodeHeader,
    hpackEncodeHeaderBlock,
    hpackDecodeHeader,
    hpackDecodeTrailer,
    hpackDiscardHeader,
    just,
    fixHeaders,
) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.ByteString.Internal (create)
import qualified Data.ByteString.Lazy as BS.Lazy
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Network.ByteOrder
import Network.HTTP.Semantics
import Network.HTTP.Types

import Imports
import Network.HPACK
import Network.HTTP2.Frame
import Network.HTTP2.H2.Context
import Network.HTTP2.H2.Types

-- $setup
-- >>> :set -XOverloadedStrings

----------------------------------------------------------------

fixHeaders :: ResponseHeaders -> ResponseHeaders
fixHeaders hdr = deleteUnnecessaryHeaders hdr

deleteUnnecessaryHeaders :: ResponseHeaders -> ResponseHeaders
deleteUnnecessaryHeaders hdr = filter del hdr
  where
    del (k, _) = k `notElem` headersToBeRemoved

headersToBeRemoved :: [HeaderName]
headersToBeRemoved =
    [ hConnection
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

-- | Encode a complete header block
--
-- Grows the buffer as needed: a header that does not fit is retried with a
-- larger buffer (the encoder does not modify the dynamic table for a header
-- it could not write).
hpackEncodeHeaderBlock
    :: Context
    -> BufferSize
    -- ^ Initial buffer size (must fit a dynamic table size update)
    -> TokenHeaderList
    -> IO BS.Lazy.ByteString
hpackEncodeHeaderBlock ctx = go hpackEncodeHeader []
  where
    go _ acc _ [] = return $ BS.Lazy.fromChunks (reverse acc)
    go enc acc siz ths = do
        (chunk, ths') <- E.bracket (mallocBytes siz) free $ \buf -> do
            (ths', len) <- enc ctx buf siz ths
            chunk <- create len $ \p -> copyBytes p buf len
            return (chunk, ths')
        if BS.null chunk
            then go enc acc (siz * 2) ths -- no progress: grow
            else go hpackEncodeHeaderLoop (chunk : acc) siz ths'

----------------------------------------------------------------

hpackDecodeHeader
    :: HeaderBlockFragment -> StreamId -> Context -> IO TokenHeaderTable
hpackDecodeHeader hdrblk sid ctx = do
    tbl@(_, vt) <- hpackDecode "illegal header" hdrblk sid ctx
    if isClient ctx || checkRequestHeader vt
        then return tbl
        else E.throwIO $ StreamErrorIsSent ProtocolError sid "illegal header"

hpackDecodeTrailer
    :: HeaderBlockFragment -> StreamId -> Context -> IO TokenHeaderTable
hpackDecodeTrailer = hpackDecode "illegal trailer"

-- | Decode a field block for a stream we no longer have, and discard the result
--
-- The block must still be decoded: it may modify the dynamic table.
hpackDiscardHeader :: HeaderBlockFragment -> StreamId -> Context -> IO ()
hpackDiscardHeader hdrblk sid ctx = void $ hpackDecode "illegal header" hdrblk sid ctx

-- | Decode a field block, reporting a block we could not get through as a
-- connection error.
--
-- The first argument says which kind of block it was, since the peer reads
-- this in the GOAWAY and "illegal trailer" about a request's headers is a
-- confusing thing to be told.
hpackDecode
    :: ReasonPhrase
    -> HeaderBlockFragment
    -> StreamId
    -> Context
    -> IO TokenHeaderTable
hpackDecode illegal hdrblk sid Context{..} =
    decodeTokenHeader decodeDynamicTable hdrblk `E.catch` handl
  where
    -- Connection errors, both of them, even though a malformed message is a
    -- stream error by RFC 9113 section 8.1.1.  Either way the field block was
    -- abandoned part-way through, so our dynamic table now holds the entries
    -- decoded before the throw and nothing after them -- no longer what the
    -- peer's encoder believes we have.  Section 10.5.1: "The field block MUST
    -- be processed to ensure a consistent connection state, unless the
    -- connection is closed."  We did not, so it must be.
    --
    -- A malformed message caught /after/ a complete decode is a different
    -- matter, and 'hpackDecodeHeader' reports those as stream errors.
    handl IllegalHeaderName =
        E.throwIO $ ConnectionErrorIsSent ProtocolError sid illegal
    handl e = do
        let msg = fromString $ show e
        E.throwIO $ ConnectionErrorIsSent CompressionError sid msg

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
    mStatus = getFieldValue tokenStatus reqvt
    mScheme = getFieldValue tokenScheme reqvt
    mPath = getFieldValue tokenPath reqvt
    mMethod = getFieldValue tokenMethod reqvt
    mConnection = getFieldValue tokenConnection reqvt
    mTE = getFieldValue tokenTE reqvt
    mAuthority = getFieldValue tokenAuthority reqvt
    mHost = getFieldValue tokenHost reqvt

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
