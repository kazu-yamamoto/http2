module Network.HPACK.HeaderBlock.String where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Network.HPACK.Huffman
import Network.HPACK.Types

encode :: HuffmanEncoding -> HeaderStuff -> [Word8]
encode he h = he $ BS.unpack h

decode :: HuffmanDecoding -> [Word8] -> HeaderStuff
decode hd ws = BS.pack $ hd ws

parseString :: HuffmanDecoding -> Bool -> Int -> [Word8] -> (HeaderStuff, [Word8])
parseString hd isHuffman len ws = (conv es, ws')
  where
    (es, ws') = splitAt len ws
    conv | isHuffman = decode hd
         | otherwise = BS.pack
