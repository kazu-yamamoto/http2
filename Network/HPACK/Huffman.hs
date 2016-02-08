module Network.HPACK.Huffman (
  -- * Type
    HuffmanEncoding
  , HuffmanDecoding
  -- * Encoding/decoding
  , encode
  , decode
  , decodeDummy
  ) where

import Network.HPACK.Huffman.Decode
import Network.HPACK.Huffman.Encode
