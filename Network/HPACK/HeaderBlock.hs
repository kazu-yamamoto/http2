-- | FIXME

module Network.HPACK.HeaderBlock (
    HeaderBlock
  , toByteStream
  , fromByteStream
  , toHeaderBlock
  , fromHeaderBlock
  ) where

import Network.HPACK.Context
import Network.HPACK.HeaderBlock.Decode
import Network.HPACK.HeaderBlock.Types
import Network.HPACK.Huffman

toByteStream :: HuffmanEncoding -> HeaderBlock -> ByteStream
toByteStream = undefined

fromByteStream :: HuffmanDecoding -> ByteStream -> HeaderBlock
fromByteStream = undefined

----------------------------------------------------------------

toHeaderBlock :: HeaderSet
              -> Context
              -> (HeaderBlock, Context)
toHeaderBlock = undefined
