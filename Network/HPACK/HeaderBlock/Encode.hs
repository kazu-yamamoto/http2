{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HPACK.HeaderBlock.Encode (
    encodeHeader,
    encodeTokenHeader,
    encodeString,
    encodeS,
) where

import Control.Exception (bracket, throwIO)
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.ByteString.Internal (create)
import Data.IORef
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (minusPtr)
import Network.ByteOrder
import Network.HTTP.Semantics

import Imports
import Network.HPACK.HeaderBlock.Integer
import Network.HPACK.Huffman
import Network.HPACK.Table
import Network.HPACK.Types

----------------------------------------------------------------

changeTableSize :: DynamicTable -> WriteBuffer -> IO ()
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

-- | Converting '[Header]' to the HPACK format.
--   This function has overhead of allocating/freeing a temporary buffer.
--   'BufferOverrun' will be thrown if the temporary buffer is too small.
encodeHeader
    :: EncodeStrategy
    -> Size
    -- ^ The size of a temporary buffer.
    -> DynamicTable
    -> [Header]
    -> IO ByteString
    -- ^ An HPACK format
encodeHeader stgy siz dyntbl hs = encodeHeader' stgy siz dyntbl hs'
  where
    mk' (k, v) = (t, v)
      where
        t = toToken $ foldedCase k
    hs' = map mk' hs

-- | Converting 'TokenHeaderList' to the HPACK format.
--   'BufferOverrun' will be thrown if the temporary buffer is too small.
encodeHeader'
    :: EncodeStrategy
    -> Size
    -- ^ The size of a temporary buffer.
    -> DynamicTable
    -> TokenHeaderList
    -> IO ByteString
    -- ^ An HPACK format
encodeHeader' stgy siz dyntbl hs = bracket (mallocBytes siz) free enc
  where
    enc buf = do
        (hs', len) <- encodeTokenHeader buf siz stgy True dyntbl hs
        case hs' of
            [] -> create len $ \p -> copyBytes p buf len
            _ -> throwIO BufferOverrun

----------------------------------------------------------------

-- | Converting 'TokenHeaderList' to the HPACK format directly in the buffer.
--
--   When calling this function for a new 'TokenHeaderList',
--   4th argument must be 'True'.
--
--   The return value is a pair of leftover 'TokenHeaderList' and
--   how many bytes are filled in the buffer.
--   If the leftover is empty, the encoding is finished.
--   Otherwise, this function should be called with it again.
--   4th argument must be 'False'.
--
--   4th argument is relating to dynamic table size update.
--   If 'True' and the limit is set by 'setLimitForEncoding',
--   dynamic table size update is generated at the beginning of
--   the HPACK format.
encodeTokenHeader
    :: Buffer
    -> BufferSize
    -> EncodeStrategy
    -> Bool
    -- ^ 'True' at the first time, 'False' when continued.
    -> DynamicTable
    -> TokenHeaderList
    -> IO (TokenHeaderList, Int)
    -- ^ Leftover, filled length
encodeTokenHeader buf siz EncodeStrategy{..} first dyntbl hs0 = do
    wbuf <- newWriteBuffer buf siz
    when first $ changeTableSize dyntbl wbuf
    let fa = indexedHeaderField dyntbl wbuf useHuffman
        fb = literalHeaderFieldWithIncrementalIndexingIndexedName dyntbl wbuf useHuffman
        fc = literalHeaderFieldWithIncrementalIndexingNewName dyntbl wbuf useHuffman
        fd = literalHeaderFieldWithoutIndexingIndexedName dyntbl wbuf useHuffman
        fe = literalHeaderFieldWithoutIndexingNewName dyntbl wbuf useHuffman
        fe' = literalHeaderFieldWithoutIndexingNewName' dyntbl wbuf useHuffman
        rev = getRevIndex dyntbl
        step0 = case compressionAlgo of
            Naive -> naiveStep fe'
            Static -> staticStep fa fd fe
            Linear -> linearStep rev fa fb fc fd
    ref1 <- currentOffset wbuf >>= newIORef
    ref2 <- newIORef hs0
    loop wbuf ref1 ref2 step0 hs0 `E.catch` \BufferOverrun -> return ()
    end <- readIORef ref1
    let len = end `minusPtr` buf
    hs <- readIORef ref2
    return (hs, len)
  where
    loop wbuf ref1 ref2 step hsx = go hsx
      where
        go [] = return ()
        go ((t, v) : hs) = do
            _ <- step t v
            currentOffset wbuf >>= writeIORef ref1
            writeIORef ref2 hs
            go hs

----------------------------------------------------------------

naiveStep
    :: (FieldName -> FieldValue -> IO ()) -> Token -> FieldValue -> IO ()
naiveStep fe t v = fe (tokenFoldedKey t) v

----------------------------------------------------------------

staticStep :: FA -> FD -> FE -> Token -> FieldValue -> IO ()
staticStep fa fd fe t v = lookupRevIndex' t v fa fd fe

----------------------------------------------------------------

linearStep :: RevIndex -> FA -> FB -> FC -> FD -> Token -> FieldValue -> IO ()
linearStep rev fa fb fc fd t v = lookupRevIndex t v fa fb fc fd rev

----------------------------------------------------------------

type FA = HIndex -> IO ()
type FB = FieldValue -> Entry -> HIndex -> IO ()
type FC = FieldName -> FieldValue -> Entry -> IO ()
type FD = FieldValue -> HIndex -> IO ()
type FE = FieldName -> FieldValue -> IO ()

-- 6.1.  Indexed Header Field Representation
-- Indexed Header Field
indexedHeaderField
    :: DynamicTable -> WriteBuffer -> Bool -> FA
indexedHeaderField dyntbl wbuf _ hidx =
    fromHIndexToIndex dyntbl hidx >>= index wbuf

-- 6.2.1.  Literal Header Field with Incremental Indexing
-- Literal Header Field with Incremental Indexing -- Indexed Name
literalHeaderFieldWithIncrementalIndexingIndexedName
    :: DynamicTable -> WriteBuffer -> Bool -> FB
literalHeaderFieldWithIncrementalIndexingIndexedName dyntbl wbuf huff v ent hidx = do
    fromHIndexToIndex dyntbl hidx >>= indexedName wbuf huff 6 set01 v
    insertEntry ent dyntbl

-- 6.2.1.  Literal Header Field with Incremental Indexing
-- Literal Header Field with Incremental Indexing -- New Name
literalHeaderFieldWithIncrementalIndexingNewName
    :: DynamicTable -> WriteBuffer -> Bool -> FC
literalHeaderFieldWithIncrementalIndexingNewName dyntbl wbuf huff k v ent = do
    newName wbuf huff set01 k v
    insertEntry ent dyntbl

-- 6.2.2.  Literal Header Field without Indexing
-- Literal Header Field without Indexing -- Indexed Name
literalHeaderFieldWithoutIndexingIndexedName
    :: DynamicTable -> WriteBuffer -> Bool -> FD
literalHeaderFieldWithoutIndexingIndexedName dyntbl wbuf huff v hidx =
    fromHIndexToIndex dyntbl hidx >>= indexedName wbuf huff 4 set0000 v

-- 6.2.2.  Literal Header Field without Indexing
-- Literal Header Field without Indexing -- New Name
literalHeaderFieldWithoutIndexingNewName
    :: DynamicTable -> WriteBuffer -> Bool -> FE
literalHeaderFieldWithoutIndexingNewName _ wbuf huff k v =
    newName wbuf huff set0000 k v

literalHeaderFieldWithoutIndexingNewName'
    :: DynamicTable -> WriteBuffer -> Bool -> FieldName -> FieldValue -> IO ()
literalHeaderFieldWithoutIndexingNewName' _ wbuf huff k v =
    newName wbuf huff set0000 k v

----------------------------------------------------------------

{-# INLINE change #-}
change :: WriteBuffer -> Int -> IO ()
change wbuf i = encodeI wbuf set001 5 i

{-# INLINE index #-}
index :: WriteBuffer -> Int -> IO ()
index wbuf i = encodeI wbuf set1 7 i

-- Using Huffman encoding
{-# INLINE indexedName #-}
indexedName
    :: WriteBuffer -> Bool -> Int -> Setter -> FieldValue -> Index -> IO ()
indexedName wbuf huff n set v idx = do
    encodeI wbuf set n idx
    encStr wbuf huff v

-- Using Huffman encoding
{-# INLINE newName #-}
newName :: WriteBuffer -> Bool -> Setter -> FieldName -> FieldValue -> IO ()
newName wbuf huff set k v = do
    write8 wbuf $ set 0
    encStr wbuf huff k
    encStr wbuf huff v

----------------------------------------------------------------

type Setter = Word8 -> Word8

-- Assuming MSBs are 0.
set1, set01, set001, set0000 :: Setter
set1 x = x `setBit` 7
set01 x = x `setBit` 6
set001 x = x `setBit` 5
-- set0001 x = x `setBit` 4 -- Never indexing
set0000 = id

----------------------------------------------------------------

-- | String encoding.
--   The algorithm based on copy avoidance and
--   selection of better result of huffman or raw.
encodeS
    :: WriteBuffer
    -> Bool
    -- ^ Use Huffman if efficient
    -> (Word8 -> Word8)
    -- ^ Setting prefix
    -> (Word8 -> Word8)
    -- ^ Setting huffman flag
    -> Int
    -- ^ N+
    -> ByteString
    -- ^ Target
    -> IO ()
encodeS wbuf False set _ n bs = do
    let len = BS.length bs
    encodeI wbuf set n len
    copyByteString wbuf bs
encodeS wbuf True set setH n bs = do
    let origLen = BS.length bs
        expectedLen = (origLen `div` 10) * 8 -- 80%: decided by examples
        expectedIntLen = integerLength n expectedLen
    ff wbuf expectedIntLen
    len <- encodeH wbuf bs
    let intLen = integerLength n len
    if origLen < len
        then do
            ff wbuf (negate (expectedIntLen + len))
            encodeI wbuf set n origLen
            copyByteString wbuf bs
        else
            if intLen == expectedIntLen
                then do
                    ff wbuf (negate (expectedIntLen + len))
                    encodeI wbuf (set . setH) n len
                    ff wbuf len
                else do
                    let gap = intLen - expectedIntLen
                    shiftLastN wbuf gap len
                    ff wbuf (negate (intLen + len))
                    encodeI wbuf (set . setH) n len
                    ff wbuf len

{-# INLINE encStr #-}
encStr :: WriteBuffer -> Bool -> ByteString -> IO ()
encStr wbuf h bs = encodeS wbuf h id (`setBit` 7) 7 bs

-- | String encoding (7+) with a temporary buffer whose size is 4096.
encodeString
    :: Bool
    -- ^ Use Huffman if efficient
    -> ByteString
    -- ^ Target
    -> IO ByteString
encodeString h bs = withWriteBuffer 4096 $ \wbuf -> encStr wbuf h bs

{-
N+   1   2     3 <- bytes
8  254 382 16638
7  126 254 16510
6   62 190 16446
5   30 158 16414
4   14 142 16398
3    6 134 16390
2    2 130 16386
1    0 128 16384
-}

{-# INLINE integerLength #-}
integerLength :: Int -> Int -> Int
integerLength 8 l
    | l <= 254 = 1
    | l <= 382 = 2
    | otherwise = 3
integerLength 7 l
    | l <= 126 = 1
    | l <= 254 = 2
    | otherwise = 3
integerLength 6 l
    | l <= 62 = 1
    | l <= 190 = 2
    | otherwise = 3
integerLength 5 l
    | l <= 30 = 1
    | l <= 158 = 2
    | otherwise = 3
integerLength 4 l
    | l <= 14 = 1
    | l <= 142 = 2
    | otherwise = 3
integerLength 3 l
    | l <= 6 = 1
    | l <= 134 = 2
    | otherwise = 3
integerLength 2 l
    | l <= 2 = 1
    | l <= 130 = 2
    | otherwise = 3
integerLength _ l
    | l <= 0 = 1
    | l <= 128 = 2
    | otherwise = 3
