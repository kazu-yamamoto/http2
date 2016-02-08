module Network.HPACK2.Huffman (
  -- * Type
    HuffmanEncoding
  , HuffmanDecoding
  -- * Encoding/decoding
  , encode
  , decode
  , decodeDummy
  ) where

import Network.HPACK2.Huffman.Decode
import Network.HPACK2.Huffman.Encode
