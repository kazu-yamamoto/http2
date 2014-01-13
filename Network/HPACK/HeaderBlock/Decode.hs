module Network.HPACK.HeaderBlock.Decode (
    fromByteStream
  ) where

import Data.Bits (testBit, clearBit, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Network.HPACK.Builder
import Network.HPACK.HeaderBlock.HeaderField
import qualified Network.HPACK.HeaderBlock.Integer as I
import qualified Network.HPACK.HeaderBlock.String as S
import Network.HPACK.Huffman
import Network.HPACK.Types

----------------------------------------------------------------

-- | Converting the low level format to 'HeaderBlock'.
fromByteStream :: HuffmanDecoding -> ByteStream
               -> Either DecodeError HeaderBlock
fromByteStream hd inp = go inp empty
  where
    go bs builder
      | BS.null bs = Right $ run builder
      | otherwise  = do
        (hf, bs') <- toHeaderField hd bs
        go bs' (builder << hf)

toHeaderField :: HuffmanDecoding -> ByteString
              -> Either DecodeError (HeaderField, ByteString)
toHeaderField hd bs
  | BS.null bs    = Left EmptyBlock
  | w `testBit` 7 = Right $ indexed w bs'
  | w `testBit` 6 = withoutIndexing hd w bs'
  | otherwise     = incrementalIndexing hd w bs'
  where
    w = BS.head bs
    bs' = BS.tail bs

----------------------------------------------------------------

indexed :: Word8 -> ByteString -> (HeaderField, ByteString)
indexed w ws = (Indexed idx , ws)
  where
    idx = fromIntegral $ clearBit w 7

withoutIndexing :: HuffmanDecoding -> Word8 -> ByteString
                -> Either DecodeError (HeaderField, ByteString)
withoutIndexing hd w ws
  | isIndexedName w = indexedName NotAdd hd w ws
  | otherwise       = newName NotAdd hd ws

incrementalIndexing :: HuffmanDecoding -> Word8 -> ByteString
                    -> Either DecodeError (HeaderField, ByteString)
incrementalIndexing hd w ws
  | isIndexedName w = indexedName Add hd w ws
  | otherwise       = newName Add hd ws

----------------------------------------------------------------

indexedName :: Indexing -> HuffmanDecoding -> Word8 -> ByteString
            -> Either DecodeError (HeaderField, ByteString)
indexedName indexing hd w ws = do
    (val,ws'') <- headerStuff hd ws'
    let hf = Literal indexing (Idx idx) val
    return (hf, ws'')
  where
    p = mask6 w
    (idx,ws') = I.parseInteger 6 p ws


newName :: Indexing -> HuffmanDecoding -> ByteString
        -> Either DecodeError (HeaderField, ByteString)
newName indexing hd ws = do
    (key,ws')  <- headerStuff hd ws
    (val,ws'') <- headerStuff hd ws'
    let hf = Literal indexing (Lit key) val
    return (hf, ws'')

----------------------------------------------------------------

headerStuff :: HuffmanDecoding -> ByteString
            -> Either DecodeError (HeaderStuff, ByteString)
headerStuff hd bs
  | BS.null bs  = Left EmptyEncodedString
  | otherwise   = S.parseString hd huff len bs''
  where
    w = BS.head bs
    bs' = BS.tail bs
    p = dropHuffman w
    huff = isHuffman w
    (len, bs'') = I.parseInteger 7 p bs'

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
