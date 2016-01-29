{-# LANGUAGE CPP, BangPatterns, RecordWildCards, OverloadedStrings #-}

module Network.HPACK2.HeaderBlock.Encode (
    HPACKEncodingOne
  , prepareEncodeHeader
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Data.Bits (setBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Network.HPACK2.Buffer
import qualified Network.HPACK2.HeaderBlock.Integer as I
import qualified Network.HPACK2.Huffman as Huffman
import Network.HPACK2.Table
import Network.HPACK2.Types

type HPACKEncodingOne = DynamicTable -> WorkingBuffer -> Header -> IO ()

type Step = Bool -> HPACKEncodingOne

prepareEncodeHeader :: EncodeStrategy -> DynamicTable -> WorkingBuffer -> IO HPACKEncodingOne
prepareEncodeHeader EncodeStrategy{..} dyntbl wbuf = do
    msiz <- needChangeTableSize dyntbl
    case msiz of
        Keep -> return ()
        Change lim -> do
            renewDynamicTable lim dyntbl
            change wbuf lim
        Ignore lim -> do
            resetLimitForEncoding dyntbl
            change wbuf lim
    return $ case compressionAlgo of
        Naive  -> naiveStep  useHuffman
        Static -> staticStep useHuffman
        Linear -> linearStep useHuffman

----------------------------------------------------------------

naiveStep :: Step
naiveStep huff _dyntbl wbuf (k,v) = newName wbuf huff set0000 k v

----------------------------------------------------------------

staticStep :: Step
staticStep huff dyntbl wbuf h@(k,v) = do
    x <- lookupTable h dyntbl
    case x of
        None                      -> newName     wbuf huff   set0000 k v
        KeyOnly  InStaticTable i  -> indexedName wbuf huff 4 set0000 i v
        KeyOnly  InDynamicTable _ -> newName     wbuf huff   set0000 k v
        KeyValue InStaticTable i  -> indexedName wbuf huff 4 set0000 i v
        KeyValue InDynamicTable _ -> newName     wbuf huff   set0000 k v

----------------------------------------------------------------
-- A simple encoding strategy to reset the reference set first
-- by 'Index 0' and uses indexing as much as possible.

linearStep :: Step
linearStep huff dyntbl wbuf h = do
    cache <- lookupTable h dyntbl
    case cache of
        None                      -> check wbuf huff dyntbl h Nothing
        KeyOnly  InStaticTable  i -> check wbuf huff dyntbl h (Just i)
        KeyOnly  InDynamicTable i -> check wbuf huff dyntbl h (Just i)
        KeyValue InStaticTable  i -> index wbuf i
        KeyValue InDynamicTable i -> index wbuf i

{-# INLINE check #-}
check :: WorkingBuffer -> Bool -> DynamicTable -> Header -> Maybe Int -> IO ()
check wbuf huff dyntbl h@(k,v) x
  | k `elem` headersNotToIndex = do
      case x of
          Nothing -> newName     wbuf huff set0000 k v
          Just i  -> indexedName wbuf huff 4 set0000 i v
  | otherwise = do
      case x of
          Nothing -> newName     wbuf huff set01 k v
          Just i  -> indexedName wbuf huff 6 set01 i v
      let e = toEntry h
      insertEntry e dyntbl


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
