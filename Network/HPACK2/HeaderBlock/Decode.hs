{-# LANGUAGE BangPatterns #-}

module Network.HPACK2.HeaderBlock.Decode (
    HPACKDecoding
  , decodeHeader
  ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Bits (testBit, clearBit, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Network.HPACK2.Builder
import qualified Network.HPACK2.HeaderBlock.Integer as I
import Network.HPACK2.Huffman
import Network.HPACK2.Table
import Network.HPACK2.Types

----------------------------------------------------------------

-- | HPACK decoding from 'ByteString' to 'HeaderList'.
type HPACKDecoding = DynamicTable -> ByteString -> IO HeaderList

-- | Converting the low level format for HTTP header to 'HeaderList'.
--   'DecodeError' would be thrown.
decodeHeader :: HPACKDecoding
decodeHeader dyntbl inp = chkChange inp
  where
    chkChange bs
      | BS.null bs          = return []
      | isTableSizeUpdate w = do
            bs' <- tableSizeUpdate dyntbl w ws
            chkChange bs'
      | otherwise           = go bs empty
      where
        w = BS.head bs
        ws = BS.tail bs
    go bs builder
      | BS.null bs = return $! run builder
      | otherwise  = do
        (!kv, bs') <- toHeader dyntbl bs
        go bs' (builder << kv)

toHeader :: DynamicTable -> ByteString -> IO (Header, ByteString)
toHeader dyntbl bs
  | BS.null bs    = throwIO EmptyBlock
  | w `testBit` 7 = indexed dyntbl w bs'
  | w `testBit` 6 = incrementalIndexing dyntbl w bs'
  | w `testBit` 5 = throwIO IllegalTableSizeUpdate
  | w `testBit` 4 = neverIndexing       dyntbl w bs'
  | otherwise     = withoutIndexing     dyntbl w bs'
  where
    w = BS.head bs
    bs' = BS.tail bs

tableSizeUpdate :: DynamicTable -> Word8 -> ByteString -> IO ByteString
tableSizeUpdate dyntbl w ws = do
    suitable <- isSuitableSize siz dyntbl
    unless suitable $ throwIO TooLargeTableSize
    renewDynamicTable siz dyntbl
    return ws'
  where
    w' = mask5 w
    (siz, ws') = I.parseInteger 5 w' ws

----------------------------------------------------------------

indexed :: DynamicTable -> Word8 -> ByteString -> IO (Header, ByteString)
indexed dyntbl w ws = do
    !kv <- fromEntry . snd <$> which dyntbl idx
    return (kv, ws')
  where
    w' = clearBit w 7
    (idx, ws') = I.parseInteger 7 w' ws

incrementalIndexing :: DynamicTable -> Word8 -> ByteString -> IO (Header, ByteString)
incrementalIndexing dyntbl w ws = do
    r@(kv,_) <- if isIndexedName1 w then
                    indexedName dyntbl w ws 6 mask6
                  else
                    newName dyntbl ws
    let !e = toEntry kv
    insertEntry e dyntbl
    return r

withoutIndexing :: DynamicTable -> Word8 -> ByteString -> IO (Header, ByteString)
withoutIndexing dyntbl w ws
  | isIndexedName2 w = indexedName dyntbl w ws 4 mask4
  | otherwise        = newName dyntbl ws

neverIndexing :: DynamicTable -> Word8 -> ByteString -> IO (Header, ByteString)
neverIndexing dyntbl w ws
  | isIndexedName2 w = indexedName dyntbl w ws 4 mask4
  | otherwise        = newName dyntbl ws

----------------------------------------------------------------

indexedName :: DynamicTable
            -> Word8 -> ByteString -> Int -> (Word8 -> Word8)
            -> IO (Header, ByteString)
indexedName dyntbl w ws n mask = do
    (!val,!ws'') <- headerStuff dyntbl ws'
    !key <- entryHeaderName . snd <$> which dyntbl idx
    return ((key,val), ws'')
  where
    p = mask w
    (idx,ws') = I.parseInteger n p ws

newName :: DynamicTable -> ByteString -> IO (Header, ByteString)
newName dyntbl ws = do
    (!key,!ws')  <- headerStuff dyntbl ws
    (!val,!ws'') <- headerStuff dyntbl ws'
    return ((key,val), ws'')

----------------------------------------------------------------

headerStuff :: DynamicTable -> ByteString -> IO (HeaderStuff, ByteString)
headerStuff dyntbl bs
  | BS.null bs  = throwIO EmptyEncodedString
  | otherwise   = parseString (huffmanDecode dyntbl) huff len bs''
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

isTableSizeUpdate :: Word8 -> Bool
isTableSizeUpdate w = w .&. 0xe0 == 0x20

----------------------------------------------------------------

isHuffman :: Word8 -> Bool
isHuffman w = w `testBit` 7

dropHuffman :: Word8 -> Word8
dropHuffman w = w `clearBit` 7

----------------------------------------------------------------

-- | Parsing 'HeaderStuff' from 'ByteString'.
--   The second 'Bool' is whether or not huffman encoding is used.
--   The third 'Int' is the length of the encoded string.

parseString :: HuffmanDecoding -> Bool -> Int -> ByteString
            -> IO (HeaderStuff, ByteString)
parseString _      False len bs = return (es, bs')
  where
    (es, bs') = BS.splitAt len bs
parseString hufdec True  len bs = hufdec es >>= \x -> return (x,bs')
  where
    (es, bs') = BS.splitAt len bs
