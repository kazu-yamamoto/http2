{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Network.HPACK.HeaderBlock.Integer (
    encode
  , decode
  , parseInteger
  ) where

import Data.Array (Array, listArray, (!))
import Data.Bits ((.&.), shiftR, testBit)
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
--
-- >>> decode 5 10 $ BS.empty
-- 10
-- >>> decode 5 31 $ BS.pack [154,10]
-- 1337
-- >>> decode 8 42 $ BS.empty
-- 42
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

----------------------------------------------------------------

{-# INLINE parseInteger #-}
-- |
--
-- >>> parseInteger 7 127 $ BS.pack [210,211,212,87,88,89,90]
-- (183839313,"XYZ")
parseInteger :: Int -> Word8 -> ReadBuffer -> IO Int
parseInteger n w rbuf
  | i < p     = return i
  | otherwise = decode n w rbuf
  where
    p = powerArray ! n
    i = fromIntegral w
