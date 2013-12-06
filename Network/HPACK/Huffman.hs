module Network.HPACK.Huffman (
  -- * Type
    ByteStream
  , HuffmanEncoding
  , HuffmanDecoding
  -- * Encoding/decoding
  , huffmanEncodingInRequest
  , huffmanDecodingInRequest
  , huffmanEncodingInResponse
  , huffmanDecodingInResponse
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.HPACK.Huffman.Request
import Network.HPACK.Huffman.Response
import Network.HPACK.Types

-- | Byte stream in HTTP request/response.
type ByteStream = ByteString

-- | Huffman encoding.
type HuffmanEncoding = HeaderStuff -> ByteStream

-- | Huffman decoding.
type HuffmanDecoding = ByteStream -> HeaderStuff

-- | Hoffman encoding in HTTP/2.0 request.
huffmanEncodingInRequest :: HuffmanEncoding
huffmanEncodingInRequest = intsToBs . encodeInRequest . bsToInts

-- | Hoffman decoding in HTTP/2.0 request.
huffmanDecodingInRequest :: HuffmanDecoding
huffmanDecodingInRequest = intsToBs . decodeInRequest . bsToInts

-- | Hoffman encoding in HTTP/2.0 response.
huffmanEncodingInResponse :: HuffmanEncoding
huffmanEncodingInResponse = intsToBs . encodeInResponse . bsToInts

-- | Hoffman decoding in HTTP/2.0 response.
huffmanDecodingInResponse :: HuffmanDecoding
huffmanDecodingInResponse = intsToBs . decodeInResponse . bsToInts

-- FIXME: should improve performance by removing intermediate data.
intsToBs :: [Int] -> ByteString
intsToBs = BS.pack . map fromIntegral

bsToInts :: ByteString -> [Int]
bsToInts = map fromIntegral . BS.unpack