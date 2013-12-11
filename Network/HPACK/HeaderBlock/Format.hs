module Network.HPACK.HeaderBlock.Format where

import Network.HPACK.HeaderBlock.HeaderField
import Network.HPACK.Huffman
import Network.HPACK.Types

----------------------------------------------------------------

-- | Converting 'HeaderBlock' to the low level format.
toByteStream :: HuffmanEncoding -> HeaderBlock -> ByteStream
toByteStream = undefined

----------------------------------------------------------------

-- | Converting the low level format to 'HeaderBlock'.
fromByteStream :: HuffmanDecoding -> ByteStream -> HeaderBlock
fromByteStream = undefined
