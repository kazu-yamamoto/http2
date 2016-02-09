{-# LANGUAGE CPP, BangPatterns, RecordWildCards, OverloadedStrings #-}

module Network.HPACK.HeaderBlock.Encode (
    HPACKEncodingOne
  , prepareEncodeHeader
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Data.Bits (setBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (readIORef)
import qualified Data.Map as M
import Data.Word (Word8)
import Network.HPACK.Buffer
import qualified Network.HPACK.HeaderBlock.Integer as I
import qualified Network.HPACK.Huffman as Huffman
import Network.HPACK.Table
import Network.HPACK.Table.Dynamic
import Network.HPACK.Table.RevIndex
import Network.HPACK.Table.Static
import Network.HPACK.Types

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
staticStep huff DynamicTable{..} wbuf (k,v) = do
    Outer rev <- readIORef revref
    case M.lookup k rev of
        Nothing -> newName     wbuf huff   set0000 k v
        Just (Inner ss ds) -> case lookup v ss of
            Just sidx -> indexedName wbuf huff 4 set0000 (fromSIndexToIndex sidx) v
            Nothing   -> case lookup v ds of
                Just _  -> newName     wbuf huff   set0000 k v
                Nothing -> case ss of
                    ((_,sidx):_) -> indexedName wbuf huff 4 set0000 (fromSIndexToIndex sidx) v
                    [] -> case ds of
                        [] -> error "staticStep"
                        _  -> newName     wbuf huff   set0000 k v
  where
    EncodeInfo revref _ = codeInfo

----------------------------------------------------------------

linearStep :: Step
linearStep huff dyntbl@DynamicTable{..} wbuf h@(k,v) = do
    Outer rev <- readIORef revref
    case M.lookup k rev of
        Nothing
         | notToIndex -> newName     wbuf huff   set0000 k v
         | otherwise  -> do
               newName     wbuf huff   set01   k v
               insertEntry (toEntry h) dyntbl
        Just (Inner ss ds) -> case lookup v ss of
            Just sidx -> index wbuf (fromSIndexToIndex sidx)
            Nothing   -> case lookup v ds of
                Just didx -> fromDIndexToIndex dyntbl didx >>= index wbuf
                Nothing   -> case ss of
                    ((_,sidx):_)
                      | notToIndex -> do
                          let !i = fromSIndexToIndex sidx
                          indexedName wbuf huff 4 set0000 i v
                      | otherwise  -> do
                          let !i = fromSIndexToIndex sidx
                          indexedName wbuf huff 6 set01   i v
                          insertEntry (toEntry h) dyntbl
                    [] -> case ds of
                        ((_,didx):_)
                          | notToIndex -> do
                              !i <- fromDIndexToIndex dyntbl didx
                              indexedName wbuf huff 4 set0000 i v
                          | otherwise  -> do
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
