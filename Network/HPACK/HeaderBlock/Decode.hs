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
import Network.HPACK.Types

----------------------------------------------------------------

-- | Converting the low level format to 'HeaderBlock'.
fromByteStream :: ByteStream -> Either DecodeError HeaderBlock
fromByteStream inp = go inp empty
  where
    go bs builder
      | BS.null bs = Right $ run builder
      | otherwise  = do
        (hf, bs') <- toHeaderField bs
        go bs' (builder << hf)

toHeaderField :: ByteString
              -> Either DecodeError (HeaderField, ByteString)
toHeaderField bs
  | BS.null bs    = Left EmptyBlock
  | w `testBit` 7 = Right $ indexed w bs'
  | w `testBit` 6 = withoutIndexing w bs'
  | otherwise     = incrementalIndexing w bs'
  where
    w = BS.head bs
    bs' = BS.tail bs

----------------------------------------------------------------

indexed :: Word8 -> ByteString -> (HeaderField, ByteString)
indexed w ws = (Indexed idx , ws')
  where
    w' = clearBit w 7
    (idx, ws') = I.parseInteger 7 w' ws

withoutIndexing :: Word8 -> ByteString
                -> Either DecodeError (HeaderField, ByteString)
withoutIndexing w ws
  | isIndexedName w = indexedName NotAdd w ws
  | otherwise       = newName NotAdd ws

incrementalIndexing :: Word8 -> ByteString
                    -> Either DecodeError (HeaderField, ByteString)
incrementalIndexing w ws
  | isIndexedName w = indexedName Add w ws
  | otherwise       = newName Add ws

----------------------------------------------------------------

indexedName :: Indexing -> Word8 -> ByteString
            -> Either DecodeError (HeaderField, ByteString)
indexedName indexing w ws = do
    (val,ws'') <- headerStuff ws'
    let hf = Literal indexing (Idx idx) val
    return (hf, ws'')
  where
    p = mask6 w
    (idx,ws') = I.parseInteger 6 p ws


newName :: Indexing -> ByteString
        -> Either DecodeError (HeaderField, ByteString)
newName indexing ws = do
    (key,ws')  <- headerStuff ws
    (val,ws'') <- headerStuff ws'
    let hf = Literal indexing (Lit key) val
    return (hf, ws'')

----------------------------------------------------------------

headerStuff :: ByteString
            -> Either DecodeError (HeaderStuff, ByteString)
headerStuff bs
  | BS.null bs  = Left EmptyEncodedString
  | otherwise   = S.parseString huff len bs''
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
