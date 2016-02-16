{-# LANGUAGE CPP, BangPatterns, RecordWildCards, OverloadedStrings #-}

module Network.HPACK.HeaderBlock.Encode (
    encodeHeader
  , encodeHeaderBuffer
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Exception (bracket, try, throwIO)
import Control.Monad (when)
import Data.Bits (setBit)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString, create, memcpy)
import Data.IORef (readIORef)
import qualified Data.HashMap.Strict as H
import Data.Word (Word8)
import Foreign.Marshal.Alloc
import Foreign.Ptr (minusPtr)
import Network.HPACK.Buffer
import qualified Network.HPACK.HeaderBlock.Integer as I
import qualified Network.HPACK.Huffman as Huffman
import Network.HPACK.Table
import Network.HPACK.Table.Dynamic
import Network.HPACK.Table.RevIndex
import Network.HPACK.Table.Static
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
    let step = case compressionAlgo of
            Naive  -> naiveStep  useHuffman
            Static -> staticStep useHuffman
            Linear -> linearStep useHuffman
    wbuf <- newWorkingBuffer buf siz
    when first $ changeTableSize dyntbl wbuf
    loop wbuf step hs0
  where
    loop wbuf _    []     = do
        end <- currentOffset wbuf
        let !len = end `minusPtr` buf
        return ([], len)
    loop wbuf step hhs@(h:hs) = do
        end <- currentOffset wbuf
        ex <- try $ step dyntbl wbuf h
        case ex of
            Right ()           -> loop wbuf step hs
            Left BufferOverrun -> do
                let !len = end `minusPtr` buf
                return (hhs,len)

----------------------------------------------------------------

naiveStep :: Bool -> DynamicTable -> WorkingBuffer -> Header -> IO ()
naiveStep huff _dyntbl wbuf (k,v) = newName wbuf huff set0000 k v

----------------------------------------------------------------

staticStep :: Bool -> DynamicTable -> WorkingBuffer -> Header -> IO ()
staticStep huff DynamicTable{..} wbuf (k,v) = do
    Outer rev <- readIORef revref
    case H.lookup k rev of
        Nothing -> newName wbuf huff set0000 k v
        Just (Inner ss ds) -> case H.lookup v ss of
            Just sidx -> indexedName wbuf huff 4 set0000 (fromSIndexToIndex sidx) v
            Nothing   -> case H.lookup v ds of
                Just _  -> newName wbuf huff set0000 k v
                Nothing -> case top ss of
                    Just sidx -> indexedName wbuf huff 4 set0000 (fromSIndexToIndex sidx) v
                    Nothing
                        | H.null ds -> error "staticStep"
                        | otherwise -> newName wbuf huff set0000 k v
  where
    EncodeInfo revref _ = codeInfo

----------------------------------------------------------------

linearStep :: Bool -> DynamicTable -> WorkingBuffer -> Header -> IO ()
linearStep huff dyntbl@DynamicTable{..} wbuf h@(k,v) = do
    Outer rev <- readIORef revref
    case H.lookup k rev of
        Nothing
         | notToIndex ->
             -- 6.2.2.  Literal Header Field without Indexing
             -- Literal Header Field without Indexing -- New Name
             newName wbuf huff set0000 k v
         | otherwise  -> do
             -- 6.2.1.  Literal Header Field with Incremental Indexing
             -- Literal Header Field with Incremental Indexing -- New Name
             newName wbuf huff set01 k v
             insertEntry (toEntry h) dyntbl
        Just (Inner ss ds) -> case H.lookup v ss of
            Just sidx -> index wbuf (fromSIndexToIndex sidx)
            Nothing   -> case H.lookup v ds of
                Just didx ->
                    -- 6.1.  Indexed Header Field Representation
                    -- Indexed Header Field
                    fromDIndexToIndex dyntbl didx >>= index wbuf
                Nothing   -> case top ss of
                    Just sidx
                      | notToIndex -> do
                          -- 6.2.2.  Literal Header Field without Indexing
                          -- Literal Header Field without Indexing -- Indexed Name
                          let !i = fromSIndexToIndex sidx
                          indexedName wbuf huff 4 set0000 i v
                      | otherwise  -> do
                          -- 6.2.1.  Literal Header Field with Incremental Indexing
                          -- Literal Header Field with Incremental Indexing -- Indexed Name
                          let !i = fromSIndexToIndex sidx
                          indexedName wbuf huff 6 set01   i v
                          insertEntry (toEntry h) dyntbl
                    Nothing -> case top ds of
                        Just didx
                          | notToIndex -> do
                              -- 6.2.2.  Literal Header Field without Indexing
                              -- Literal Header Field without Indexing -- Indexed Name

                              !i <- fromDIndexToIndex dyntbl didx
                              indexedName wbuf huff 4 set0000 i v
                          | otherwise  -> do
                              -- 6.2.1.  Literal Header Field with Incremental Indexing
                              -- Literal Header Field with Incremental Indexing -- Indexed Name
                              !i <- fromDIndexToIndex dyntbl didx
                              indexedName wbuf huff 6 set01   i v
                              insertEntry (toEntry h) dyntbl
                        _ -> error "linearStep"
  where
    EncodeInfo revref _ = codeInfo
    notToIndex = k `elem` headersNotToIndex

headersNotToIndex :: [HeaderName]
headersNotToIndex = [
    ":path"
  , "content-length"
  , "location"
  , "etag"
  , "set-cookie"
  ]

----------------------------------------------------------------

{-# INLINE change #-}
change :: WorkingBuffer -> Int -> IO ()
change wbuf i = I.encode wbuf set001 5 i

{-# INLINE index #-}
index :: WorkingBuffer -> Int -> IO ()
index wbuf i = I.encode wbuf set1 7 i

-- Using Huffman encoding
{-# INLINE indexedName #-}
indexedName :: WorkingBuffer -> Bool -> Int -> Setter -> Int -> HeaderValue -> IO ()
indexedName wbuf huff n set idx v = do
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
