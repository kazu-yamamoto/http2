module Network.HPACK.HeaderBlock.String (
    encode
  , parseString
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Network.HPACK.Huffman as Huffman
import Network.HPACK.Types

-- | Encoding 'HeaderStuff' to 'ByteString' according to 'HuffmanEncoding'.
encode :: Bool -> HeaderStuff -> ByteString
encode True h = Huffman.encode h
encode _    h = h

-- fixme: removing
-- | Decoding 'ByteString' to 'HeaderStuff' according to 'HuffmanDecoding'.
decode :: ByteString -> Either DecodeError HeaderStuff
decode ws = Huffman.decode ws

-- | Parsing 'HeaderStuff' from 'ByteString'.
--   The second 'Bool' is whether or not huffman encoding is used.
--   The third 'Int' is the length of the encoded string.
parseString :: Bool -> Int -> ByteString
            -> Either DecodeError (HeaderStuff, ByteString)
parseString False len bs = Right (es, bs')
  where
    (es, bs') = BS.splitAt len bs
parseString True  len bs = decode es >>= \x -> return (x,bs')
  where
    (es, bs') = BS.splitAt len bs
