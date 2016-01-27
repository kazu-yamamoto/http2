{-# LANGUAGE BangPatterns #-}

module Network.HPACK2.HeaderBlock.Decode (
    HPACKDecoding
  , decodeHeaderWithWorkingBuffer
  ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Bits (testBit, clearBit, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Network.HPACK2.Builder
import qualified Network.HPACK2.HeaderBlock.Integer as I
import qualified Network.HPACK2.HeaderBlock.String as S
import Network.HPACK2.Table
import Network.HPACK2.Types

----------------------------------------------------------------

-- | HPACK decoding from 'ByteString' to 'HeaderList'.
type HPACKDecoding = DynamicTable -> ByteString -> IO HeaderList

-- | Converting the low level format for HTTP header to 'HeaderList'.
--   A working buffer is used for Huffman decoding.
--   'DecodeError' would be thrown.
decodeHeaderWithWorkingBuffer :: Buffer -> BufferSize -> HPACKDecoding
decodeHeaderWithWorkingBuffer buf siz dyntbl inp = chkChange inp
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
        (!kv, bs') <- toHeader buf siz dyntbl bs
        go bs' (builder << kv)

toHeader :: Buffer -> BufferSize -> DynamicTable -> ByteString -> IO (Header, ByteString)
toHeader buf siz dyntbl bs
  | BS.null bs    = throwIO EmptyBlock
  | w `testBit` 7 = indexed dyntbl w bs'
  | w `testBit` 6 = incrementalIndexing buf siz dyntbl w bs'
  | w `testBit` 5 = throwIO IllegalTableSizeUpdate
  | w `testBit` 4 = neverIndexing       buf siz dyntbl w bs'
  | otherwise     = withoutIndexing     buf siz dyntbl w bs'
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

incrementalIndexing :: Buffer -> BufferSize -> DynamicTable -> Word8 -> ByteString -> IO (Header, ByteString)
incrementalIndexing buf siz dyntbl w ws = do
    r@(kv,_) <- if isIndexedName1 w then
                    indexedName buf siz dyntbl w ws 6 mask6
                  else
                    newName buf siz ws
    let !e = toEntry kv
    insertEntry e dyntbl
    return r

withoutIndexing :: Buffer -> BufferSize -> DynamicTable -> Word8 -> ByteString -> IO (Header, ByteString)
withoutIndexing buf siz dyntbl w ws
  | isIndexedName2 w = indexedName buf siz dyntbl w ws 4 mask4
  | otherwise        = newName buf siz ws

neverIndexing :: Buffer -> BufferSize -> DynamicTable -> Word8 -> ByteString -> IO (Header, ByteString)
neverIndexing buf siz dyntbl w ws
  | isIndexedName2 w = indexedName buf siz dyntbl w ws 4 mask4
  | otherwise        = newName buf siz ws

----------------------------------------------------------------

indexedName :: Buffer -> BufferSize -> DynamicTable
            -> Word8 -> ByteString -> Int -> (Word8 -> Word8)
            -> IO (Header, ByteString)
indexedName buf siz dyntbl w ws n mask = do
    (!val,!ws'') <- headerStuff buf siz ws'
    !key <- entryHeaderName . snd <$> which dyntbl idx
    return ((key,val), ws'')
  where
    p = mask w
    (idx,ws') = I.parseInteger n p ws

newName :: Buffer -> BufferSize -> ByteString
        -> IO (Header, ByteString)
newName buf siz ws = do
    (!key,!ws')  <- headerStuff buf siz ws
    (!val,!ws'') <- headerStuff buf siz ws'
    return ((key,val), ws'')

----------------------------------------------------------------

headerStuff :: Buffer -> BufferSize -> ByteString -> IO (HeaderStuff, ByteString)
headerStuff buf siz bs
  | BS.null bs  = throwIO EmptyEncodedString
  | otherwise   = S.parseString buf siz huff len bs''
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
