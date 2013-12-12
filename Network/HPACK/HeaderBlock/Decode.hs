module Network.HPACK.HeaderBlock.Decode (
    fromByteStream
  ) where

import Data.Bits
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Network.HPACK.HeaderBlock.HeaderField
import qualified Network.HPACK.HeaderBlock.Integer as I
import qualified Network.HPACK.HeaderBlock.String as S
import Network.HPACK.Huffman
import Network.HPACK.Types

----------------------------------------------------------------

-- | Converting the low level format to 'HeaderBlock'.
fromByteStream :: HuffmanDecoding -> ByteStream -> HeaderBlock
fromByteStream hd bs = go $ BS.unpack bs
  where
    go [] = []
    go ws = hf : go ws'
       where
         (hf, ws') = toHeaderField hd ws

toHeaderField :: HuffmanDecoding -> [Word8] -> (HeaderField, [Word8])
toHeaderField _  []     = error "toHeaderField"
toHeaderField hd (w:ws)
  | testBit w 7 = indexed w ws
  | testBit w 6 = withoutIndexing hd w ws
  | otherwise   = incrementalIndexing hd w ws

indexed :: Word8 -> [Word8] -> (HeaderField, [Word8])
indexed w ws = (Indexed idx , ws)
  where
    idx = fromIntegral $ clearBit w 7

withoutIndexing :: HuffmanDecoding -> Word8 -> [Word8] -> (HeaderField, [Word8])
withoutIndexing hd w ws
  | isIndexedName w = fromIndexedName NotAdd hd w ws
  | otherwise       = fromNewName NotAdd hd ws

incrementalIndexing :: HuffmanDecoding -> Word8 -> [Word8] -> (HeaderField, [Word8])
incrementalIndexing hd w ws
  | isIndexedName w = fromIndexedName Add hd w ws
  | otherwise       = fromNewName Add hd ws

isIndexedName :: Word8 -> Bool
isIndexedName w = w .&. 63 /= 0

fromIndexedName :: Indexing -> HuffmanDecoding -> Word8 -> [Word8] -> (HeaderField, [Word8])
fromIndexedName indexing hd w ws = (hf, ws')
  where
    idx = index6 w
    (val,ws') = headerStuff hd ws
    hf = Literal indexing (Idx idx) val

fromNewName :: Indexing -> HuffmanDecoding -> [Word8] -> (HeaderField, [Word8])
fromNewName indexing hd ws = (hf, ws'')
  where
    (val,ws')  = headerStuff hd ws
    (key,ws'') = headerStuff hd ws'
    name = toHeaderName key
    hf = Literal indexing (Lit name) val

index6 :: Word8 -> Int
index6 w = fromIntegral $ w .&. 63

headerStuff :: HuffmanDecoding -> [Word8] -> (HeaderStuff, [Word8])
headerStuff = undefined
 -- see if Huffman
 -- extract len
 -- splitAt len
 -- decoding
