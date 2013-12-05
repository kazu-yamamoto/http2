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

import Control.Arrow (first)
import Network.HPACK.Context
import Network.HPACK.HeaderBlock
import Network.HPACK.Huffman
import Network.HPACK.Decode

----------------------------------------------------------------

encodeRequestHeader :: HeaderSet
                    -> Context
                    -> (ByteStream, Context)
encodeRequestHeader hs ctx =
    first (toByteStream huffmanEncodingInRequest) $ toHeaderBlock hs ctx

decodeRequestHeader :: ByteStream
                    -> Context
                    -> Maybe (HeaderSet, Context)
decodeRequestHeader bs ctx =
    fromHeaderBlock (fromByteStream huffmanDecodingInRequest bs) ctx

----------------------------------------------------------------

encodeResponseHeader :: HeaderSet
                     -> Context
                     -> (ByteStream, Context)
encodeResponseHeader hs ctx =
    first (toByteStream huffmanEncodingInResponse) $ toHeaderBlock hs ctx

decodeResponseHeader :: ByteStream
                    -> Context
                    -> Maybe (HeaderSet, Context)
decodeResponseHeader bs ctx =
    fromHeaderBlock (fromByteStream huffmanDecodingInResponse bs) ctx

----------------------------------------------------------------

toHeaderBlock :: HeaderSet
              -> Context
              -> (HeaderBlock, Context)
toHeaderBlock = undefined
