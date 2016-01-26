{-# LANGUAGE BangPatterns #-}

module Network.HPACK2.HeaderBlock.Decode (
    decodeHeader
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

-- | Converting the low level format for HTTP header to 'HeaderList'.
--   'DecodeError' would be thrown.
decodeHeader :: DynamicTable -> ByteString -> Buffer -> BufferSize -> IO HeaderList
decodeHeader dyntbl inp buf siz = chkChange inp
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
        (!kv, bs') <- toHeader dyntbl bs buf siz
        go bs' (builder << kv)

toHeader :: DynamicTable -> ByteString -> Buffer -> BufferSize -> IO (Header, ByteString)
toHeader dyntbl bs buf siz
  | BS.null bs    = throwIO EmptyBlock
  | w `testBit` 7 = indexed dyntbl w bs'
  | w `testBit` 6 = incrementalIndexing dyntbl w bs' buf siz
  | w `testBit` 5 = throwIO IllegalTableSizeUpdate
  | w `testBit` 4 = neverIndexing dyntbl w bs' buf siz
  | otherwise     = withoutIndexing dyntbl w bs' buf siz
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

incrementalIndexing :: DynamicTable -> Word8 -> ByteString -> Buffer -> BufferSize -> IO (Header, ByteString)
incrementalIndexing dyntbl w ws buf siz = do
    r@(kv,_) <- if isIndexedName1 w then
                    indexedName dyntbl w ws 6 mask6 buf siz
                  else
                    newName ws buf siz
    let !e = toEntry kv
    insertEntry e dyntbl
    return r

withoutIndexing :: DynamicTable -> Word8 -> ByteString -> Buffer -> BufferSize -> IO (Header, ByteString)
withoutIndexing dyntbl w ws buf siz
  | isIndexedName2 w = indexedName dyntbl w ws 4 mask4 buf siz
  | otherwise        = newName ws buf siz

neverIndexing :: DynamicTable -> Word8 -> ByteString -> Buffer -> BufferSize -> IO (Header, ByteString)
neverIndexing dyntbl w ws buf siz
  | isIndexedName2 w = indexedName dyntbl w ws 4 mask4 buf siz
  | otherwise        = newName ws buf siz

----------------------------------------------------------------

indexedName :: DynamicTable -> Word8 -> ByteString -> Int -> (Word8 -> Word8)
            -> Buffer -> BufferSize
            -> IO (Header, ByteString)
indexedName dyntbl w ws n mask buf siz = do
    (!val,!ws'') <- headerStuff ws' buf siz
    !key <- entryHeaderName . snd <$> which dyntbl idx
    return ((key,val), ws'')
  where
    p = mask w
    (idx,ws') = I.parseInteger n p ws

newName :: ByteString -> Buffer -> BufferSize
        -> IO (Header, ByteString)
newName ws buf siz = do
    (!key,!ws')  <- headerStuff ws buf siz
    (!val,!ws'') <- headerStuff ws' buf siz
    return ((key,val), ws'')

----------------------------------------------------------------

headerStuff :: ByteString -> Buffer -> BufferSize -> IO (HeaderStuff, ByteString)
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

isTableSizeUpdate :: Word8 -> Bool
isTableSizeUpdate w = w .&. 0xe0 == 0x20

----------------------------------------------------------------

isHuffman :: Word8 -> Bool
isHuffman w = w `testBit` 7

dropHuffman :: Word8 -> Word8
dropHuffman w = w `clearBit` 7
