module Network.HPACK.Huffman (
  -- * Huffman encoding/decoding
    encodeH
  , encodeHuffman
  , decodeH
  , decodeHuffman
  , HuffmanDecoder
  , decH
  ) where

import Network.HPACK.Huffman.Decode
import Network.HPACK.Huffman.Encode
