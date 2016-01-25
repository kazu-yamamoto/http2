module Network.HPACK2.HeaderBlock.String (
    encode
  , parseString
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Network.HPACK2.Huffman as Huffman
import Network.HPACK2.Types

-- | Encoding 'HeaderStuff' to 'ByteString' according to 'HuffmanEncoding'.
encode :: Bool -> HeaderStuff -> ByteString
encode True h = Huffman.encode h
encode _    h = h

-- | Parsing 'HeaderStuff' from 'ByteString'.
--   The second 'Bool' is whether or not huffman encoding is used.
--   The third 'Int' is the length of the encoded string.
parseString :: Bool -> Int -> ByteString
            -> Either DecodeError (HeaderStuff, ByteString)
parseString False len bs = Right (es, bs')
  where
    (es, bs') = BS.splitAt len bs
parseString True  len bs = Huffman.decode es >>= \x -> return (x,bs')
  where
    (es, bs') = BS.splitAt len bs
