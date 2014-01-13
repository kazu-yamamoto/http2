module Network.HPACK.HeaderBlock.String where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.HPACK.Huffman
import Network.HPACK.Types

-- | Encoding 'HeaderStuff' to 'ByteString' according to 'HuffmanEncoding'.
encode :: HuffmanEncoding -> HeaderStuff -> ByteString
encode he h = he h

-- | Decoding 'ByteString' to 'HeaderStuff' according to 'HuffmanDecoding'.
decode :: HuffmanDecoding -> ByteString -> Either DecodeError HeaderStuff
decode hd ws = hd ws

-- | Parsing 'HeaderStuff' from 'ByteString'.
--   The second 'Bool' is whether or not huffman encoding is used.
--   The third 'Int' is the length of the encoded string.
parseString :: HuffmanDecoding -> Bool -> Int -> ByteString
            -> Either DecodeError (HeaderStuff, ByteString)
parseString _  False len bs = Right (es, bs')
  where
    (es, bs') = BS.splitAt len bs
parseString hd True  len bs = decode hd es >>= \x -> return (x,bs')
  where
    (es, bs') = BS.splitAt len bs
