-- | FIXME

module Network.HPACK (
  -- * Type
    ByteStream
  , HeaderSet
  , EncodeContenxt
  , DecodeContenxt
  -- * Request
  , encodeRequestHeader
  , decodeRequestHeader
  -- * Response
  , encodeResponseHeader
  , decodeResponseHeader
  ) where

import Control.Arrow (first)
import Network.HPACK.HeaderBlock
import Network.HPACK.Huffman

data HeaderSet
data EncodeContenxt
data DecodeContenxt

----------------------------------------------------------------

encodeRequestHeader :: HeaderSet
                    -> EncodeContenxt
                    -> (ByteStream, EncodeContenxt)
encodeRequestHeader hs ctx =
    first (toByteStream huffmanEncodingInRequest) $ toHeaderBlock hs ctx

decodeRequestHeader :: ByteStream
                    -> DecodeContenxt
                    -> Maybe (HeaderSet, DecodeContenxt)
decodeRequestHeader bs ctx =
    fromHeaderBlock (fromByteStream huffmanDecodingInRequest bs) ctx

----------------------------------------------------------------

encodeResponseHeader :: HeaderSet
                     -> EncodeContenxt
                     -> (ByteStream, EncodeContenxt)
encodeResponseHeader hs ctx =
    first (toByteStream huffmanEncodingInResponse) $ toHeaderBlock hs ctx

decodeResponseHeader :: ByteStream
                    -> DecodeContenxt
                    -> Maybe (HeaderSet, DecodeContenxt)
decodeResponseHeader bs ctx =
    fromHeaderBlock (fromByteStream huffmanDecodingInResponse bs) ctx

----------------------------------------------------------------

toHeaderBlock :: HeaderSet
              -> EncodeContenxt
              -> (HeaderBlock, EncodeContenxt)
toHeaderBlock = undefined

fromHeaderBlock :: HeaderBlock
                -> DecodeContenxt
                -> Maybe (HeaderSet, DecodeContenxt)
fromHeaderBlock = undefined
