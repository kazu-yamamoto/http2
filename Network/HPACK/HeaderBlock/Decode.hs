module Network.HPACK.HeaderBlock.Decode (
    fromByteStream
  ) where

import Data.Bits (testBit, clearBit, (.&.))
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
  | w `testBit` 7 = indexed w ws
  | w `testBit` 6 = withoutIndexing hd w ws
  | otherwise     = incrementalIndexing hd w ws

----------------------------------------------------------------

indexed :: Word8 -> [Word8] -> (HeaderField, [Word8])
indexed w ws = (Indexed idx , ws)
  where
    idx = fromIntegral $ clearBit w 7

withoutIndexing :: HuffmanDecoding -> Word8 -> [Word8] -> (HeaderField, [Word8])
withoutIndexing hd w ws
  | isIndexedName w = indexedName NotAdd hd w ws
  | otherwise       = newName NotAdd hd ws

incrementalIndexing :: HuffmanDecoding -> Word8 -> [Word8] -> (HeaderField, [Word8])
incrementalIndexing hd w ws
  | isIndexedName w = indexedName Add hd w ws
  | otherwise       = newName Add hd ws

----------------------------------------------------------------

indexedName :: Indexing -> HuffmanDecoding -> Word8 -> [Word8] -> (HeaderField, [Word8])
indexedName indexing hd w ws = (hf, ws')
  where
    idx = index6 w
    (val,ws') = headerStuff hd ws
    hf = Literal indexing (Idx idx) val

newName :: Indexing -> HuffmanDecoding -> [Word8] -> (HeaderField, [Word8])
newName indexing hd ws = (hf, ws'')
  where
    (key,ws')  = headerStuff hd ws
    (val,ws'') = headerStuff hd ws'
    name = toHeaderName key
    hf = Literal indexing (Lit name) val

----------------------------------------------------------------

headerStuff :: HuffmanDecoding -> [Word8] -> (HeaderStuff, [Word8])
headerStuff _  []     = error "headerStuff"
headerStuff hd (w:ws) = (hs, ws'')
  where
    p = dropHuffman w
    huff = isHuffman w
    (len, ws') = I.parseInteger 7 p ws
    (hs, ws'') = S.parseString hd huff len ws'

----------------------------------------------------------------

mask6 :: Word8 -> Word8
mask6 w = w .&. 63

isIndexedName :: Word8 -> Bool
isIndexedName w = mask6 w /= 0

index6 :: Word8 -> Int
index6 = fromIntegral . mask6

----------------------------------------------------------------

isHuffman :: Word8 -> Bool
isHuffman w = w `testBit` 7

dropHuffman :: Word8 -> Word8
dropHuffman w = w `clearBit` 7
