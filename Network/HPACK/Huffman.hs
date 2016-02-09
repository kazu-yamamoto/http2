module Network.HPACK.Huffman (
  -- * Type
    HuffmanEncoding
  , HuffmanDecoding
  -- * Encoding/decoding
  , encode
  , encodeHuffman
  , decode
  , decodeDummy
  , decodeHuffman
  ) where

import Network.HPACK.Huffman.Decode
import Network.HPACK.Huffman.Encode
