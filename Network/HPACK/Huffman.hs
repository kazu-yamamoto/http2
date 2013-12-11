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

import Network.HPACK.Huffman.Request
import Network.HPACK.Huffman.Response
import Network.HPACK.Huffman.Code
