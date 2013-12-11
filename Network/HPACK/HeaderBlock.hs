-- | FIXME

module Network.HPACK.HeaderBlock (
    module Network.HPACK.HeaderBlock.HeaderField
  , toByteStream
  , fromByteStream
  , toHeaderBlock
  , fromHeaderBlock
  ) where

import Network.HPACK.HeaderBlock.Decode
import Network.HPACK.HeaderBlock.Encode
import Network.HPACK.HeaderBlock.HeaderField
import Network.HPACK.Huffman
import Network.HPACK.Types

toByteStream :: HuffmanEncoding -> HeaderBlock -> ByteStream
toByteStream = undefined

fromByteStream :: HuffmanDecoding -> ByteStream -> HeaderBlock
fromByteStream = undefined
