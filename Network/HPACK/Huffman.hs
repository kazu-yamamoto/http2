module Network.HPACK.Huffman (
  -- * Type
    HuffmanEncoding
  , HuffmanDecoding
  -- * Encoding/decoding
  , encode
  , decode
  ) where

import Network.HPACK.Huffman.Decode
import Network.HPACK.Huffman.Encode
