module Network.HPACK2.HeaderBlock.String (
    encode
  ) where

import Data.ByteString (ByteString)
import qualified Network.HPACK2.Huffman as Huffman
import Network.HPACK2.Types

-- | Encoding 'HeaderStuff' to 'ByteString' according to 'HuffmanEncoding'.
encode :: Bool -> HeaderStuff -> ByteString
encode True h = Huffman.encode h
encode _    h = h
