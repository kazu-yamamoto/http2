{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Network.HPACK.HeaderBlock.Integer (
    encode
  , encodeInteger
  , decode
  , decodeInteger
  , parseInteger
  ) where

import Data.Array (Array, listArray, (!))
import Data.Bits ((.&.), shiftR, testBit)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Network.HPACK.Buffer

-- $setup
-- >>> import qualified Data.ByteString as BS

powerArray :: Array Int Int
powerArray = listArray (1,8) [1,3,7,15,31,63,127,255]

----------------------------------------------------------------

{-
if I < 2^N - 1, encode I on N bits
   else
       encode (2^N - 1) on N bits
       I = I - (2^N - 1)
       while I >= 128
            encode (I % 128 + 128) on 8 bits
            I = I / 128
       encode I on 8 bits
-}

encode :: WorkingBuffer -> (Word8 -> Word8) -> Int -> Int -> IO ()
encode wbuf set n i
  | i < p     = writeWord8 wbuf $ set $ fromIntegral i
  | otherwise = do
        writeWord8 wbuf $ set $ fromIntegral p
        encode' wbuf (i - p)
  where
    p = powerArray ! n

encode' :: WorkingBuffer -> Int -> IO ()
encode' wbuf i
  | i < 128   = writeWord8 wbuf $ fromIntegral i
  | otherwise = do
        writeWord8 wbuf $ fromIntegral (r + 128)
        encode' wbuf q
  where
    q = i `shiftR` 7
    r = i .&. 0x7f

encodeInteger :: Int -> Int -> IO ByteString
encodeInteger n i = withTemporaryBuffer 4096 $ \wbuf -> encode wbuf id n i

----------------------------------------------------------------

{-
decode I from the next N bits
   if I < 2^N - 1, return I
   else
       M = 0
       repeat
           B = next octet
           I = I + (B & 127) * 2^M
           M = M + 7
       while B & 128 == 128
       return I
-}

{-# INLINE decode #-}
-- | Integer decoding. The first argument is N of prefix.
decode :: Int -> Word8 -> ReadBuffer -> IO Int
decode n w rbuf
  | i < p     = return i
  | otherwise = decode' rbuf 0 i
  where
    p = powerArray ! n
    i = fromIntegral w

{-# INLINE decode' #-}
decode' :: ReadBuffer -> Int -> Int -> IO Int
decode' rbuf m i = do
    !b <- fromIntegral <$> getByte rbuf
    let !i' = i + (b .&. 0x7f) * 2 ^ m
        !m' = m + 7
        !cont = b `testBit` 7
    if cont then decode' rbuf m' i' else return i'

-- | Integer decoding. The first argument is N of prefix.
--
-- >>> decodeInteger 5 10 $ BS.empty
-- 10
-- >>> decodeInteger 5 31 $ BS.pack [154,10]
-- 1337
-- >>> decodeInteger 8 42 $ BS.empty
-- 42
decodeInteger :: Int -> Word8 -> ByteString -> IO Int
decodeInteger n w bs = withReadBuffer bs $ \rbuf -> decode n w rbuf

----------------------------------------------------------------

{-# INLINE parseInteger #-}
parseInteger :: Int -> Word8 -> ReadBuffer -> IO Int
parseInteger n w rbuf
  | i < p     = return i
  | otherwise = decode n w rbuf
  where
    p = powerArray ! n
    i = fromIntegral w
