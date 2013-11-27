module Network.HPACK (
    encodeInRequest
  , decodeInRequest
  ) where

import Network.HPACK.Huffman
import Network.HPACK.HuffmanRequest

encoderForRequest :: Encoder
encoderForRequest = toEncoder huffmanRequest

-- | Hoffman encoding in HTTP/2.0 request.
encodeInRequest :: [Int] -> [Int]
encodeInRequest = encode encoderForRequest

decoderForRequest :: Decoder
decoderForRequest = toDecoder huffmanRequest

-- | Hoffman decoding in HTTP/2.0 request.
decodeInRequest :: [Int] -> [Int]
decodeInRequest = decode decoderForRequest
