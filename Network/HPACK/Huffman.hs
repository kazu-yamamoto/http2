module Network.HPACK.Huffman (
  -- * Type
    HuffmanEncoding
  , HuffmanDecoding
  -- * Encoding/decoding
  , huffmanEncodeInRequest
  , huffmanDecodeInRequest
  , huffmanEncodeInResponse
  , huffmanDecodeInResponse
  ) where

import Network.HPACK.Huffman.Decode
import Network.HPACK.Huffman.Encode
import Network.HPACK.Huffman.Request
import Network.HPACK.Huffman.Response
