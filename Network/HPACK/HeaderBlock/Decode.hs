{-# LANGUAGE BangPatterns, CPP #-}

module Network.HPACK.HeaderBlock.Decode (
    decodeHeader
  , decodeHeaderTable
  , ValueTable
  , toHeaderTable
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
import Network.HPACK.Token
import Network.HPACK.Types

import Data.Array (Array)
import qualified Data.Array.IO as IOA
import qualified Data.Array.Unsafe as Unsafe

type ValueTable = Array Int (Maybe HeaderValue)

----------------------------------------------------------------

-- | Converting the HPACK format to 'HeaderList'.
--   'DecodeError' would be thrown if the HPACK format is broken.
--   'BufferOverrun' will be thrown if the temporary buffer for Huffman decoding is too small.
decodeHeader :: DynamicTable
             -> ByteString -- ^ An HPACK format
             -> IO HeaderList
decodeHeader dyntbl inp = decodeHPACK dyntbl inp decodeSimple

decodeHeaderTable :: DynamicTable
                  -> ByteString -- ^ An HPACK format
                  -> IO (TokenHeaderList, ValueTable)
decodeHeaderTable dyntbl inp = decodeHPACK dyntbl inp decodeSophisticated

decodeHPACK :: DynamicTable
            -> ByteString
            -> (DynamicTable -> ReadBuffer -> IO a)
            -> IO a
decodeHPACK dyntbl inp dec = withReadBuffer inp chkChange
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
                dec dyntbl rbuf
          else
            throwIO HeaderBlockTruncated

decodeSimple :: DynamicTable -> ReadBuffer -> IO HeaderList
decodeSimple dyntbl rbuf = go empty
  where
    go builder = do
        more <- hasOneByte rbuf
        if more then do
            w <- getByte rbuf
            !tv <- toTokenHeader dyntbl w rbuf
            let builder' = builder << tv
            go builder'
          else do
            let !tvs = run builder
                !kvs = map (\(t,v) -> let !k = tokenFoldedKey t in (k,v)) tvs
            return kvs

decodeSophisticated :: DynamicTable -> ReadBuffer
                    -> IO (TokenHeaderList, ValueTable)
decodeSophisticated dyntbl rbuf = do
    -- using otherToken to reduce condition
    arr <- IOA.newArray (minToken,otherToken) Nothing
    !tvs <- pseudoNormal arr
    tbl <- Unsafe.unsafeFreeze arr
    return (tvs, tbl)
  where
    pseudoNormal :: IOA.IOArray Int (Maybe HeaderValue) -> IO TokenHeaderList
    pseudoNormal arr = pseudo
      where
        pseudo = do
            more <- hasOneByte rbuf
            if more then do
                w <- getByte rbuf
                tv@(!t,!v) <- toTokenHeader dyntbl w rbuf
                IOA.writeArray arr (toIx t) (Just v)
                if isPseudo t then
                    pseudo
                  else do
                    let builder = empty << tv
                    normal builder
              else
                return []
        normal builder = do
            more <- hasOneByte rbuf
            if more then do
                w <- getByte rbuf
                tv@(!t,!v) <- toTokenHeader dyntbl w rbuf
                IOA.writeArray arr (toIx t) (Just v)
                if isPseudo t then
                    throwIO IllegalPseudoHeader
                  else do -- fixme :: cookie
                    let builder' = builder << tv
                    normal builder'
              else do
                let !tvs = run builder
                return tvs

toTokenHeader :: DynamicTable -> Word8 -> ReadBuffer -> IO TokenHeader
toTokenHeader dyntbl w rbuf
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

indexed :: DynamicTable -> Word8 -> ReadBuffer -> IO TokenHeader
indexed dyntbl w rbuf = do
    let !w' = clearBit w 7
    !idx <- I.decode 7 w' rbuf
    entryTokenHeader <$> toIndexedEntry dyntbl idx

incrementalIndexing :: DynamicTable -> Word8 -> ReadBuffer -> IO TokenHeader
incrementalIndexing dyntbl w rbuf = do
    tv@(t,v) <- if isIndexedName1 w then
                    indexedName dyntbl w rbuf 6 mask6
                else
                    newName dyntbl rbuf
    let !e = toEntryToken t v
    insertEntry e dyntbl
    return tv

withoutIndexing :: DynamicTable -> Word8 -> ReadBuffer -> IO TokenHeader
withoutIndexing dyntbl w rbuf
  | isIndexedName2 w = indexedName dyntbl w rbuf 4 mask4
  | otherwise        = newName dyntbl rbuf

neverIndexing :: DynamicTable -> Word8 -> ReadBuffer -> IO TokenHeader
neverIndexing dyntbl w rbuf
  | isIndexedName2 w = indexedName dyntbl w rbuf 4 mask4
  | otherwise        = newName dyntbl rbuf

----------------------------------------------------------------

indexedName :: DynamicTable -> Word8 -> ReadBuffer
            -> Int -> (Word8 -> Word8)
            -> IO TokenHeader
indexedName dyntbl w rbuf n mask = do
    let !p = mask w
    !idx <- I.decode n p rbuf
    !t <- entryToken <$> toIndexedEntry dyntbl idx
    !val <- headerStuff dyntbl rbuf
    let !tv = (t,val)
    return tv

newName :: DynamicTable -> ReadBuffer -> IO TokenHeader
newName dyntbl rbuf = do
    !t <- toToken <$> headerStuff dyntbl rbuf
    !val <- headerStuff dyntbl rbuf
    let !tv = (t,val)
    return tv

----------------------------------------------------------------

headerStuff :: DynamicTable -> ReadBuffer -> IO HeaderStuff
headerStuff dyntbl rbuf = do
    more <- hasOneByte rbuf
    if more then do
        w <- getByte rbuf
        let !p = dropHuffman w
            !huff = isHuffman w
        !len <- I.decode 7 p rbuf
        decodeString huff (huffmanDecoder dyntbl) rbuf len
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

decodeString :: Bool -> HuffmanDecoding -> ReadBuffer -> Int -> IO HeaderStuff
decodeString huff hufdec rbuf len = do
    more <- hasMoreBytes rbuf len
    if more then
        if huff then
            hufdec rbuf len
          else
            extractByteString rbuf len
      else
        throwIO HeaderBlockTruncated


----------------------------------------------------------------

toHeaderTable :: HeaderList -> IO (TokenHeaderList, ValueTable)
toHeaderTable kvs = do
    arr <- IOA.newArray (minToken,otherToken) Nothing
    !tvs <- conv arr
    tbl <- Unsafe.unsafeFreeze arr
    return (tvs, tbl)
  where
    conv :: IOA.IOArray Int (Maybe HeaderValue) -> IO TokenHeaderList
    conv arr = go kvs empty
      where
        go :: HeaderList -> Builder TokenHeader -> IO TokenHeaderList
        go []         builder = return $ run builder
        go ((k,v):xs) builder = do
            let !t = toToken k
            IOA.writeArray arr (toIx t) (Just v)
            let !tv = (t,v)
                !builder' = builder << tv
            go xs builder'

