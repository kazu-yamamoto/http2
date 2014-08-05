module Network.HPACK.HeaderBlock.Decode (
    fromByteStream
  , fromByteStreamDebug
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

-- | Converting the low level format to 'HeaderBlock'.
--   'HeaderBlock' forms a pair with corresponding 'ByteString'.
fromByteStreamDebug :: ByteStream -> Either DecodeError [(ByteString,HeaderField)]
fromByteStreamDebug inp = go inp empty
  where
    go bs builder
      | BS.null bs = Right $ run builder
      | otherwise  = do
        (hf, bs') <- toHeaderField bs
        let len = BS.length bs - BS.length bs'
            consumed = BS.take len bs
        go bs' (builder << (consumed,hf))

toHeaderField :: ByteString
              -> Either DecodeError (HeaderField, ByteString)
toHeaderField bs
  | BS.null bs    = Left EmptyBlock
  | w `testBit` 7 = indexed w bs'
  | w `testBit` 6 = incrementalIndexing w bs'
  | w `testBit` 5 = maxSize w bs'
  | w `testBit` 4 = neverIndexing w bs'
  | otherwise     = withoutIndexing w bs'
  where
    w = BS.head bs
    bs' = BS.tail bs

----------------------------------------------------------------

indexed :: Word8 -> ByteString -> Either DecodeError (HeaderField, ByteString)
indexed w ws = Right (Indexed idx , ws')
  where
    w' = clearBit w 7
    (idx, ws') = I.parseInteger 7 w' ws

incrementalIndexing :: Word8 -> ByteString
                    -> Either DecodeError (HeaderField, ByteString)
incrementalIndexing w ws
  | isIndexedName1 w = indexedName Add w ws 6 mask6
  | otherwise        = newName Add ws

maxSize :: Word8 -> ByteString -> Either DecodeError (HeaderField, ByteString)
maxSize w ws = Right (ChangeTableSize siz, ws')
  where
    w' = mask5 w
    (siz, ws') = I.parseInteger 5 w' ws

withoutIndexing :: Word8 -> ByteString
                -> Either DecodeError (HeaderField, ByteString)
withoutIndexing w ws
  | isIndexedName2 w = indexedName NotAdd w ws 4 mask4
  | otherwise        = newName NotAdd ws

neverIndexing :: Word8 -> ByteString
                -> Either DecodeError (HeaderField, ByteString)
neverIndexing w ws
  | isIndexedName2 w = indexedName Never w ws 4 mask4
  | otherwise        = newName Never ws

----------------------------------------------------------------

indexedName :: Indexing -> Word8 -> ByteString -> Int -> (Word8 -> Word8)
            -> Either DecodeError (HeaderField, ByteString)
indexedName indexing w ws n mask = do
    (val,ws'') <- headerStuff ws'
    let hf = Literal indexing (Idx idx) val
    return (hf, ws'')
  where
    p = mask w
    (idx,ws') = I.parseInteger n p ws

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

mask5 :: Word8 -> Word8
mask5 w = w .&. 31

mask4 :: Word8 -> Word8
mask4 w = w .&. 15

isIndexedName1 :: Word8 -> Bool
isIndexedName1 w = mask6 w /= 0

isIndexedName2 :: Word8 -> Bool
isIndexedName2 w = mask4 w /= 0

----------------------------------------------------------------

isHuffman :: Word8 -> Bool
isHuffman w = w `testBit` 7

dropHuffman :: Word8 -> Word8
dropHuffman w = w `clearBit` 7
