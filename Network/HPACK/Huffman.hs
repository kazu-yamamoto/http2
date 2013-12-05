-- | FIXME

module Network.HPACK.Huffman where

import Data.ByteString (ByteString)

type ByteStream = ByteString
data HeaderStuff -- ByteString
type HuffmanEncoding = HeaderStuff -> ByteStream
type HuffmanDecoding = ByteStream -> HeaderStuff

huffmanEncodingInRequest :: HuffmanEncoding
huffmanEncodingInRequest = undefined

huffmanDecodingInRequest :: HuffmanDecoding
huffmanDecodingInRequest = undefined

huffmanEncodingInResponse :: HuffmanEncoding
huffmanEncodingInResponse = undefined

huffmanDecodingInResponse :: HuffmanDecoding
huffmanDecodingInResponse = undefined
