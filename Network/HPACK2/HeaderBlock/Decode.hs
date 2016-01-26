module Network.HPACK2.HeaderBlock.Decode (
    decodeByteString
  , decodeByteStringDebug
  ) where

import Control.Exception (throwIO)
import Data.Bits (testBit, clearBit, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Network.HPACK2.Builder
import Network.HPACK2.HeaderBlock.HeaderField
import qualified Network.HPACK2.HeaderBlock.Integer as I
import qualified Network.HPACK2.HeaderBlock.String as S
import Network.HPACK2.Types

----------------------------------------------------------------

-- | Converting the low level format to 'HeaderBlock'.
decodeByteString :: ByteString -> Buffer -> BufferSize -> IO HeaderBlock
decodeByteString inp buf siz = go inp empty
  where
    go bs builder
      | BS.null bs = return $! run builder
      | otherwise  = do
        (hf, bs') <- toHeaderField bs buf siz
        go bs' (builder << hf)

-- | Converting the low level format to 'HeaderBlock'.
--   'HeaderBlock' forms a pair with corresponding 'ByteString'.
decodeByteStringDebug :: ByteString -> Buffer -> BufferSize -> IO [(ByteString,HeaderField)]
decodeByteStringDebug inp buf siz = go inp empty
  where
    go bs builder
      | BS.null bs = return $! run builder
      | otherwise  = do
        (hf, bs') <- toHeaderField bs buf siz
        let len = BS.length bs - BS.length bs'
            consumed = BS.take len bs
        go bs' (builder << (consumed,hf))

toHeaderField :: ByteString -> Buffer -> BufferSize -> IO (HeaderField, ByteString)
toHeaderField bs buf siz
  | BS.null bs    = throwIO EmptyBlock
  | w `testBit` 7 = indexed w bs'
  | w `testBit` 6 = incrementalIndexing w bs' buf siz
  | w `testBit` 5 = maxSize w bs'
  | w `testBit` 4 = neverIndexing w bs' buf siz
  | otherwise     = withoutIndexing w bs' buf siz
  where
    w = BS.head bs
    bs' = BS.tail bs

----------------------------------------------------------------

indexed :: Word8 -> ByteString -> IO (HeaderField, ByteString)
indexed w ws = return (Indexed idx , ws')
  where
    w' = clearBit w 7
    (idx, ws') = I.parseInteger 7 w' ws

incrementalIndexing :: Word8 -> ByteString -> Buffer -> BufferSize -> IO (HeaderField, ByteString)
incrementalIndexing w ws buf siz
  | isIndexedName1 w = indexedName Add w ws 6 mask6 buf siz
  | otherwise        = newName Add ws buf siz

maxSize :: Word8 -> ByteString -> IO (HeaderField, ByteString)
maxSize w ws = return (ChangeTableSize siz, ws')
  where
    w' = mask5 w
    (siz, ws') = I.parseInteger 5 w' ws

withoutIndexing :: Word8 -> ByteString -> Buffer -> BufferSize -> IO (HeaderField, ByteString)
withoutIndexing w ws buf siz
  | isIndexedName2 w = indexedName NotAdd w ws 4 mask4 buf siz
  | otherwise        = newName NotAdd ws buf siz

neverIndexing :: Word8 -> ByteString -> Buffer -> BufferSize -> IO (HeaderField, ByteString)
neverIndexing w ws buf siz
  | isIndexedName2 w = indexedName Never w ws 4 mask4 buf siz
  | otherwise        = newName Never ws buf siz

----------------------------------------------------------------

indexedName :: Indexing -> Word8 -> ByteString -> Int -> (Word8 -> Word8)
            -> Buffer -> BufferSize
            -> IO (HeaderField, ByteString)
indexedName indexing w ws n mask buf siz = do
    (val,ws'') <- headerStuff ws' buf siz
    let hf = Literal indexing (Idx idx) val
    return (hf, ws'')
  where
    p = mask w
    (idx,ws') = I.parseInteger n p ws

newName :: Indexing -> ByteString -> Buffer -> BufferSize
        -> IO (HeaderField, ByteString)
newName indexing ws buf siz = do
    (key,ws')  <- headerStuff ws buf siz
    (val,ws'') <- headerStuff ws' buf siz
    let hf = Literal indexing (Lit key) val
    return (hf, ws'')

----------------------------------------------------------------

headerStuff :: ByteString -> Buffer -> BufferSize
            -> IO (HeaderStuff, ByteString)
headerStuff bs buf siz
  | BS.null bs  = throwIO EmptyEncodedString
  | otherwise   = S.parseString huff len bs'' buf siz
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
