-- | FIXME

module Network.HPACK.HeaderBlock (
    module Network.HPACK.HeaderBlock.Representation
  , toByteStream
  , fromByteStream
  , toHeaderBlock
  , fromHeaderBlock
  ) where

import Network.HPACK.HeaderBlock.Encode
import Network.HPACK.HeaderBlock.Decode
import Network.HPACK.HeaderBlock.Representation
import Network.HPACK.Huffman

toByteStream :: HuffmanEncoding -> HeaderBlock -> ByteStream
toByteStream = undefined

fromByteStream :: HuffmanDecoding -> ByteStream -> HeaderBlock
fromByteStream = undefined
