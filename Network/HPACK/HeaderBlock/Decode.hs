{-# LANGUAGE BangPatterns, CPP #-}

module Network.HPACK.HeaderBlock.Decode (
    decodeHeader
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Bits (testBit, clearBit, (.&.))
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Network.HPACK.Buffer
import Network.HPACK.Builder
import qualified Network.HPACK.HeaderBlock.Integer as I
import Network.HPACK.Huffman
import Network.HPACK.Table
import Network.HPACK.Types

----------------------------------------------------------------

-- | Converting the HPACK format to 'HeaderList'.
--   'DecodeError' would be thrown if the HPACK format is broken.
--   'BufferOverrun' will be thrown if the temporary buffer for Huffman decoding is too small.
decodeHeader :: DynamicTable
             -> ByteString -- ^ An HPACK format
             -> IO HeaderList
decodeHeader dyntbl inp = withReadBuffer inp $ \rbuf -> chkChange rbuf
  where
    chkChange rbuf = do
        more <- hasOneByte rbuf
        if more then do
            w <- getByte rbuf
            if isTableSizeUpdate w then do
                tableSizeUpdate dyntbl w rbuf
                chkChange rbuf
              else do
                rewindOneByte rbuf
                go rbuf empty
          else
            return []
    go rbuf builder = do
        more <- hasOneByte rbuf
        if more then do
            w <- getByte rbuf
            !kv <- toHeader dyntbl w rbuf
            let builder' = builder << kv
            go rbuf builder'
          else
            return $! run builder

toHeader :: DynamicTable -> Word8 -> ReadBuffer -> IO Header
toHeader dyntbl w rbuf
  | w `testBit` 7 = indexed             dyntbl w rbuf
  | w `testBit` 6 = incrementalIndexing dyntbl w rbuf
  | w `testBit` 5 = throwIO IllegalTableSizeUpdate
  | w `testBit` 4 = neverIndexing       dyntbl w rbuf
  | otherwise     = withoutIndexing     dyntbl w rbuf

tableSizeUpdate :: DynamicTable -> Word8 -> ReadBuffer -> IO ()
tableSizeUpdate dyntbl w rbuf = do
    let !w' = mask5 w
    !siz <- I.decode 5 w' rbuf
    suitable <- isSuitableSize siz dyntbl
    unless suitable $ throwIO TooLargeTableSize
    renewDynamicTable siz dyntbl

----------------------------------------------------------------

indexed :: DynamicTable -> Word8 -> ReadBuffer -> IO Header
indexed dyntbl w rbuf = do
    let !w' = clearBit w 7
    !idx <- I.decode 7 w' rbuf
    fromEntry <$> which dyntbl idx

incrementalIndexing :: DynamicTable -> Word8 -> ReadBuffer -> IO Header
incrementalIndexing dyntbl w rbuf = do
    kv <- if isIndexedName1 w then
              indexedName dyntbl w rbuf 6 mask6
            else
              newName dyntbl rbuf
    let !e = toEntry kv
    insertEntry e dyntbl
    return kv

withoutIndexing :: DynamicTable -> Word8 -> ReadBuffer -> IO Header
withoutIndexing dyntbl w rbuf
  | isIndexedName2 w = indexedName dyntbl w rbuf 4 mask4
  | otherwise        = newName dyntbl rbuf

neverIndexing :: DynamicTable -> Word8 -> ReadBuffer -> IO Header
neverIndexing dyntbl w rbuf
  | isIndexedName2 w = indexedName dyntbl w rbuf 4 mask4
  | otherwise        = newName dyntbl rbuf

----------------------------------------------------------------

indexedName :: DynamicTable -> Word8 -> ReadBuffer
            -> Int -> (Word8 -> Word8)
            -> IO Header
indexedName dyntbl w rbuf n mask = do
    let !p = mask w
    !idx <- I.decode n p rbuf
    !key <- entryHeaderName <$> which dyntbl idx
    !val <- headerStuff dyntbl rbuf
    let !kv = (key,val)
    return kv

newName :: DynamicTable -> ReadBuffer -> IO Header
newName dyntbl rbuf = do
    !key <- headerStuff dyntbl rbuf
    !val <- headerStuff dyntbl rbuf
    let !kv = (key,val)
    return kv

----------------------------------------------------------------

headerStuff :: DynamicTable -> ReadBuffer -> IO HeaderStuff
headerStuff dyntbl rbuf = do
    more <- hasOneByte rbuf
    if more then do
        w <- getByte rbuf
        let !p = dropHuffman w
            !huff = isHuffman w
        !len <- I.decode 7 p rbuf
        parseString huff (huffmanDecoder dyntbl) rbuf len
      else
        throwIO EmptyEncodedString

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

parseString :: Bool -> HuffmanDecoding -> ReadBuffer -> Int -> IO HeaderStuff
parseString huff hufdec rbuf len = do
    more <- hasMoreBytes rbuf len
    if more then
        if huff then
            hufdec rbuf len
          else
            extractByteString rbuf len
      else
        throwIO HeaderBlockTruncated
