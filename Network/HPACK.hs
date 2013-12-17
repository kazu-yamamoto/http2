module Network.HPACK (
  -- * Type
    ByteStream
  , HeaderSet
  , Context
  , newContext
  , DecodeError(..)
  -- * Request
  , encodeRequestHeader
  , decodeRequestHeader
  -- * Response
  , encodeResponseHeader
  , decodeResponseHeader
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Network.HPACK.Context
import Network.HPACK.HeaderBlock
import Network.HPACK.Huffman
import Network.HPACK.Types

----------------------------------------------------------------

-- | Converting 'HeaderSet' for HTTP request to the low level format.
encodeRequestHeader :: HeaderSet
                    -> Context
                    -> IO (ByteStream, Context)
encodeRequestHeader hs ctx =
    first (toByteStream huffmanEncodeInRequest) <$> toHeaderBlock hs ctx

-- | Converting the low level format for HTTP request to 'HeaderSet'.
--   'DecodeError' would be thrown.
decodeRequestHeader :: ByteStream
                    -> Context
                    -> IO (HeaderSet, Context)
decodeRequestHeader bs ctx =
    fromHeaderBlock (fromByteStream huffmanDecodeInRequest bs) ctx

----------------------------------------------------------------

-- | Converting 'HeaderSet' for HTTP response to the low level format.
encodeResponseHeader :: HeaderSet
                     -> Context
                     -> IO (ByteStream, Context)
encodeResponseHeader hs ctx =
    first (toByteStream huffmanEncodeInResponse) <$> toHeaderBlock hs ctx

-- | Converting the low level format for HTTP response to'HeaderSet'.
--   'DecodeError' would be thrown.
decodeResponseHeader :: ByteStream
                    -> Context
                    -> IO (HeaderSet, Context)
decodeResponseHeader bs ctx =
    fromHeaderBlock (fromByteStream huffmanDecodeInResponse bs) ctx
