module Network.HPACK.HeaderBlock.String where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Network.HPACK.Huffman
import Network.HPACK.Types

encode :: HuffmanEncoding -> HeaderStuff -> [Word8]
encode he h = he $ BS.unpack h

decode :: HuffmanDecoding -> [Word8] -> Either DecodeError HeaderStuff
decode hd ws = hd ws >>= return . BS.pack

parseString :: HuffmanDecoding -> Bool -> Int -> [Word8]
            -> Either DecodeError (HeaderStuff, [Word8])
parseString _  False len ws = Right (BS.pack es, ws')
  where
    (es, ws') = splitAt len ws
parseString hd True  len ws = decode hd es >>= \x -> return (x,ws')
  where
    (es, ws') = splitAt len ws
