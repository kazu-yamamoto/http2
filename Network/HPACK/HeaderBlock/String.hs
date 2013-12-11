module Network.HPACK.HeaderBlock.String where

import Data.Word (Word8)
import Network.HPACK.Huffman
import Network.HPACK.Types

encode :: HuffmanEncoding -> HeaderStuff -> (Int,[Word8])
encode = undefined

decode :: HuffmanDecoding -> [Word8] -> HeaderStuff
decode = undefined
