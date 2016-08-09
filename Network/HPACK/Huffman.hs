module Network.HPACK.Huffman (
  -- * Type
    HuffmanEncoding
  , HuffmanDecoding
  -- * Encoding/decoding
  , encode
  , encodeHuffman
  , decode
  , decodeHuffman
  , getSize
  ) where

import Network.HPACK.Huffman.Decode
import Network.HPACK.Huffman.Encode
