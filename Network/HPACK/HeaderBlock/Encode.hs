{-# LANGUAGE CPP, BangPatterns, RecordWildCards, OverloadedStrings #-}

module Network.HPACK.HeaderBlock.Encode (
    encodeHeader
  , encodeHeaderBuffer
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Exception (bracket, throwIO)
import qualified Control.Exception as E
import Control.Monad (when)
import Data.Bits (setBit)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString, create, memcpy)
import Data.Word (Word8)
import Foreign.Marshal.Alloc
import Foreign.Ptr (minusPtr)
import Network.HPACK.Buffer
import qualified Network.HPACK.HeaderBlock.Integer as I
import qualified Network.HPACK.Huffman as Huffman
import Network.HPACK.Table
import Network.HPACK.Types

----------------------------------------------------------------

changeTableSize :: DynamicTable -> WorkingBuffer -> IO ()
changeTableSize dyntbl wbuf = do
    msiz <- needChangeTableSize dyntbl
    case msiz of
        Keep -> return ()
        Change lim -> do
            renewDynamicTable lim dyntbl
            change wbuf lim
        Ignore lim -> do
            resetLimitForEncoding dyntbl
            change wbuf lim

----------------------------------------------------------------

-- | Converting 'HeaderList' to the HPACK format.
--   'BufferOverrun' will be thrown if the temporary buffer is too small.
encodeHeader :: EncodeStrategy
             -> Size -- ^ The size of a temporary buffer.
             -> DynamicTable
             -> HeaderList
             -> IO ByteString -- ^ An HPACK format
encodeHeader stgy siz dyntbl hs = bracket (mallocBytes siz) free enc
  where
    enc buf = do
        (hs',len) <- encodeHeaderBuffer buf siz stgy True dyntbl hs
        case hs' of
            [] -> create len $ \p -> memcpy p buf len
            _  -> throwIO BufferOverrun

----------------------------------------------------------------

-- | Converting 'HeaderList' to the HPACK format directly in the buffer.
--
--   4th argument is relating to dynamic table size update.
--   When calling this function for a new 'HeaderList',
--   it must be 'True'.
--   If 'True' and set by 'setLimitForEncoding',
--   dynamic table size update is generated at the beginning of
--   the HPACK format.
--
--   If the buffer for encoding is small, leftover 'HeaderList' will
--   be returned. In this case, this function should be called with it
--   again. 4th argument must be 'False'.
--
encodeHeaderBuffer :: Buffer
                   -> BufferSize
                   -> EncodeStrategy
                   -> Bool -- ^ 'True' at the first time, 'False' when continued.
                   -> DynamicTable
                   -> HeaderList
                   -> IO (HeaderList, Int) -- ^ Leftover 'HeaderList' and the number of filled bytes.
encodeHeaderBuffer buf siz EncodeStrategy{..} first dyntbl hs0 = do
    wbuf <- newWorkingBuffer buf siz
    when first $ changeTableSize dyntbl wbuf
    let fa = indexedHeaderField dyntbl wbuf useHuffman
        fb = literalHeaderFieldWithIncrementalIndexingIndexedName dyntbl wbuf useHuffman
        fc = literalHeaderFieldWithIncrementalIndexingNewName dyntbl wbuf useHuffman
        fd = literalHeaderFieldWithoutIndexingIndexedName dyntbl wbuf useHuffman
        fe = literalHeaderFieldWithoutIndexingNewName dyntbl wbuf useHuffman
        fe' = literalHeaderFieldWithoutIndexingNewName' dyntbl wbuf useHuffman
        rev = getRevIndex dyntbl
        step = case compressionAlgo of
            Naive  -> naiveStep  fe'
            Static -> staticStep rev fa fd fe
            Linear -> linearStep rev fa fb fc fd
    loop wbuf step hs0
  where
    loop wbuf _    []     = do
        end <- currentOffset wbuf
        let !len = end `minusPtr` buf
        return ([], len)
    loop wbuf step hhs@(h:hs) = do
        end <- currentOffset wbuf
        cont <- (step h >> return True) `E.catch` \BufferOverrun -> return False
        if cont then
            loop wbuf step hs
          else do
            let !len = end `minusPtr` buf
            return (hhs,len)

----------------------------------------------------------------

naiveStep :: FE' -> Header -> IO ()
naiveStep fe (k,v) = fe k v

----------------------------------------------------------------

staticStep :: RevIndex -> FA -> FD -> FE -> Header -> IO ()
staticStep rev fa fd fe h = lookupRevIndex h fa fd fe fd rev

----------------------------------------------------------------

linearStep :: RevIndex -> FA -> FB -> FC -> FD -> Header -> IO ()
linearStep rev fa fb fc fd h = lookupRevIndex h fa fb fc fd rev

----------------------------------------------------------------

type FA = HIndex -> IO ()
type FB = HeaderValue -> Entry -> HIndex -> IO ()
type FC = HeaderName -> HeaderValue -> Entry -> IO ()
type FD = HeaderValue -> Entry -> HIndex -> IO ()
type FE = HeaderName -> HeaderValue -> Entry -> IO ()
type FE' = HeaderName -> HeaderValue -> IO ()

-- 6.1.  Indexed Header Field Representation
-- Indexed Header Field
indexedHeaderField
    :: DynamicTable -> WorkingBuffer -> Bool -> FA
indexedHeaderField dyntbl wbuf _ hidx =
    fromHIndexToIndex dyntbl hidx >>= index wbuf

-- 6.2.1.  Literal Header Field with Incremental Indexing
-- Literal Header Field with Incremental Indexing -- Indexed Name
literalHeaderFieldWithIncrementalIndexingIndexedName
    :: DynamicTable -> WorkingBuffer -> Bool -> FB
literalHeaderFieldWithIncrementalIndexingIndexedName dyntbl wbuf huff v ent hidx = do
    fromHIndexToIndex dyntbl hidx >>= indexedName wbuf huff 6 set01 v
    insertEntry ent dyntbl

-- 6.2.1.  Literal Header Field with Incremental Indexing
-- Literal Header Field with Incremental Indexing -- New Name
literalHeaderFieldWithIncrementalIndexingNewName
    :: DynamicTable -> WorkingBuffer -> Bool -> FC
literalHeaderFieldWithIncrementalIndexingNewName dyntbl wbuf huff k v ent = do
    newName wbuf huff set01 k v
    insertEntry ent dyntbl

-- 6.2.2.  Literal Header Field without Indexing
-- Literal Header Field without Indexing -- Indexed Name
literalHeaderFieldWithoutIndexingIndexedName
    :: DynamicTable -> WorkingBuffer -> Bool -> FB
literalHeaderFieldWithoutIndexingIndexedName dyntbl wbuf huff v _ hidx =
    fromHIndexToIndex dyntbl hidx >>= indexedName wbuf huff 4 set0000 v

-- 6.2.2.  Literal Header Field without Indexing
-- Literal Header Field without Indexing -- New Name
literalHeaderFieldWithoutIndexingNewName
    :: DynamicTable -> WorkingBuffer -> Bool -> FE
literalHeaderFieldWithoutIndexingNewName _ wbuf huff k v _ =
    newName wbuf huff set0000 k v

literalHeaderFieldWithoutIndexingNewName'
    :: DynamicTable -> WorkingBuffer -> Bool -> HeaderName -> HeaderValue -> IO ()
literalHeaderFieldWithoutIndexingNewName' _ wbuf huff k v =
    newName wbuf huff set0000 k v

----------------------------------------------------------------

{-# INLINE change #-}
change :: WorkingBuffer -> Int -> IO ()
change wbuf i = I.encode wbuf set001 5 i

{-# INLINE index #-}
index :: WorkingBuffer -> Int -> IO ()
index wbuf i = I.encode wbuf set1 7 i

-- Using Huffman encoding
{-# INLINE indexedName #-}
indexedName :: WorkingBuffer -> Bool -> Int -> Setter -> HeaderValue -> Index -> IO ()
indexedName wbuf huff n set v idx = do
    I.encode wbuf set n idx
    encodeString huff v wbuf

-- Using Huffman encoding
{-# INLINE newName #-}
newName :: WorkingBuffer -> Bool -> Setter -> HeaderName -> HeaderValue -> IO ()
newName wbuf huff set k v = do
    writeWord8 wbuf $ set 0
    encodeString huff k wbuf
    encodeString huff v wbuf

----------------------------------------------------------------

type Setter = Word8 -> Word8

-- Assuming MSBs are 0.
set1, set01, set001, set0000, setH :: Setter
set1    x = x `setBit` 7
set01   x = x `setBit` 6
set001  x = x `setBit` 5
-- set0001 x = x `setBit` 4 -- Never indexing
set0000 = id
setH = set1

----------------------------------------------------------------

{-# INLINE encodeString #-}
encodeString :: Bool -> ByteString -> WorkingBuffer -> IO ()
encodeString False bs wbuf = do
    let !len = BS.length bs
    I.encode wbuf id 7 len
    copyByteString wbuf bs
encodeString True  bs wbuf = do
    let !origLen = BS.length bs
        !expectedLen = (origLen `div` 10) * 8 -- 80%: decided by examples
        !expectedIntLen = integerLength expectedLen
    wind wbuf expectedIntLen
    len <- Huffman.encode wbuf bs
    let !intLen = integerLength len
    if intLen == expectedIntLen then do
        wind wbuf (negate (expectedIntLen + len))
        I.encode wbuf setH 7 len
        wind wbuf len
      else do
        let !gap = intLen - expectedIntLen
        shiftLastN wbuf gap len
        wind wbuf (negate (intLen + len))
        I.encode wbuf setH 7 len
        wind wbuf len

-- For 7+:
-- 1 byte:    0 -   126
-- 2 bytes: 127 -   254
-- 3 bytes: 255 - 16510
{-# INLINE integerLength #-}
integerLength :: Int -> Int
integerLength n
    | n <= 126  = 1
    | n <= 254  = 2
    | otherwise = 3
