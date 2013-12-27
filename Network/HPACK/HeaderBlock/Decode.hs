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
fromByteStream :: HuffmanDecoding -> ByteStream
               -> Either DecodeError HeaderBlock
fromByteStream hd bs = go (BS.unpack bs) id
  where
    go [] builder = Right $ builder []
    go ws builder = do
        (hf, ws') <- toHeaderField hd ws
        go ws' (builder . (hf :))

toHeaderField :: HuffmanDecoding -> [Word8]
              -> Either DecodeError (HeaderField, [Word8])
toHeaderField _  []     = error "toHeaderField"
toHeaderField hd (w:ws)
  | w `testBit` 7 = Right $ indexed w ws
  | w `testBit` 6 = withoutIndexing hd w ws
  | otherwise     = incrementalIndexing hd w ws

----------------------------------------------------------------

indexed :: Word8 -> [Word8] -> (HeaderField, [Word8])
indexed w ws = (Indexed idx , ws)
  where
    idx = fromIntegral $ clearBit w 7

withoutIndexing :: HuffmanDecoding -> Word8 -> [Word8]
                -> Either DecodeError (HeaderField, [Word8])
withoutIndexing hd w ws
  | isIndexedName w = indexedName NotAdd hd w ws
  | otherwise       = newName NotAdd hd ws

incrementalIndexing :: HuffmanDecoding -> Word8 -> [Word8]
                    -> Either DecodeError (HeaderField, [Word8])
incrementalIndexing hd w ws
  | isIndexedName w = indexedName Add hd w ws
  | otherwise       = newName Add hd ws

----------------------------------------------------------------

indexedName :: Indexing -> HuffmanDecoding -> Word8 -> [Word8]
            -> Either DecodeError (HeaderField, [Word8])
indexedName indexing hd w ws = do
    (val,ws'') <- headerStuff hd ws'
    let hf = Literal indexing (Idx idx) val
    return (hf, ws'')
  where
    p = mask6 w
    (idx,ws') = I.parseInteger 6 p ws


newName :: Indexing -> HuffmanDecoding -> [Word8]
        -> Either DecodeError (HeaderField, [Word8])
newName indexing hd ws = do
    (key,ws')  <- headerStuff hd ws
    (val,ws'') <- headerStuff hd ws'
    let hf = Literal indexing (Lit key) val
    return (hf, ws'')

----------------------------------------------------------------

headerStuff :: HuffmanDecoding -> [Word8]
            -> Either DecodeError (HeaderStuff, [Word8])
headerStuff _  []     = Left FIXME
headerStuff hd (w:ws) = S.parseString hd huff len ws'
  where
    p = dropHuffman w
    huff = isHuffman w
    (len, ws') = I.parseInteger 7 p ws

----------------------------------------------------------------

mask6 :: Word8 -> Word8
mask6 w = w .&. 63

isIndexedName :: Word8 -> Bool
isIndexedName w = mask6 w /= 0

----------------------------------------------------------------

isHuffman :: Word8 -> Bool
isHuffman w = w `testBit` 7

dropHuffman :: Word8 -> Word8
dropHuffman w = w `clearBit` 7
