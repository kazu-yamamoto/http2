module Network.HPACK.HeaderBlock.String where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.HPACK.Huffman
import Network.HPACK.Types

encode :: HuffmanEncoding -> HeaderStuff -> ByteString
encode he h = he h

decode :: HuffmanDecoding -> ByteString -> Either DecodeError HeaderStuff
decode hd ws = hd ws

parseString :: HuffmanDecoding -> Bool -> Int -> ByteString
            -> Either DecodeError (HeaderStuff, ByteString)
parseString _  False len bs = Right (es, bs')
  where
    (es, bs') = BS.splitAt len bs
parseString hd True  len bs = decode hd es >>= \x -> return (x,bs')
  where
    (es, bs') = BS.splitAt len bs
