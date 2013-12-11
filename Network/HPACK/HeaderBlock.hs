-- | FIXME

module Network.HPACK.HeaderBlock (
  -- * Types for header block
    module Network.HPACK.HeaderBlock.HeaderField
  -- * Header block from/to Low level
  , toByteStream
  , fromByteStream
  -- * Header block from/to header set
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
