-- | FIXME

module Network.HPACK (
  -- * Type
    ByteStream
  , HeaderSet
  , Context
  , newContext
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

----------------------------------------------------------------

encodeRequestHeader :: HeaderSet
                    -> Context
                    -> IO (ByteStream, Context)
encodeRequestHeader hs ctx =
    first (toByteStream huffmanEncodingInRequest) <$> toHeaderBlock hs ctx

decodeRequestHeader :: ByteStream
                    -> Context
                    -> IO (HeaderSet, Context)
decodeRequestHeader bs ctx =
    fromHeaderBlock (fromByteStream huffmanDecodingInRequest bs) ctx

----------------------------------------------------------------

encodeResponseHeader :: HeaderSet
                     -> Context
                     -> IO (ByteStream, Context)
encodeResponseHeader hs ctx =
    first (toByteStream huffmanEncodingInResponse) <$> toHeaderBlock hs ctx

decodeResponseHeader :: ByteStream
                    -> Context
                    -> IO (HeaderSet, Context)
decodeResponseHeader bs ctx =
    fromHeaderBlock (fromByteStream huffmanDecodingInResponse bs) ctx
