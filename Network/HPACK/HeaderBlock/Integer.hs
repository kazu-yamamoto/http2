module Network.HPACK.HeaderBlock.Integer (
    encode
  , encodeOne
  , decode
  , decodeOne
  , parseInteger
  ) where

import Data.Array (Array, listArray, (!))
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)

-- $setup
-- >>> import qualified Data.ByteString as BS

----------------------------------------------------------------

powerArray :: Array Int Int
powerArray = listArray (1,8) [1,3,7,15,31,63,127,255]

----------------------------------------------------------------

-- | Integer encoding. The first argument is N of prefix.
--
-- >>> encode 5 10
-- [10]
-- >>> encode 5 1337
-- [31,154,10]
-- >>> encode 8 42
-- [42]
encode :: Int -> Int -> [Word8]
encode n i
  | i < p     = fromIntegral i : []
  | otherwise = fromIntegral p : encode' (i - p)
  where
    p = powerArray ! n

encode' :: Int -> [Word8]
encode' i
  | i < 128   = fromIntegral i : []
  | otherwise = fromIntegral (r + 128) : encode' q
  where
--    (q,r) = i `divMod` 128
    q = i `shiftR` 7
    r = i .&. 0x7f

----------------------------------------------------------------

-- | Integer encoding.
encodeOne :: Int -> Word8
encodeOne = fromIntegral

----------------------------------------------------------------

-- | Integer decoding. The first argument is N of prefix.
--
-- >>> decode 5 10 $ BS.empty
-- 10
-- >>> decode 5 31 $ BS.pack [154,10]
-- 1337
-- >>> decode 8 42 $ BS.empty
-- 42
decode :: Int -> Word8 -> ByteString -> Int
decode n w bs
  | i < p      = i
  | BS.null bs = error $ "decode: n = " ++ show n ++ ", w = " ++ show w ++ ", bs = empty"
  | otherwise  = BS.foldr' (\x y -> fromIntegral x - 128 + y * 128) i0 bs' + i
  where
    p = powerArray ! n
    i = fromIntegral w
    i0 = fromIntegral $ BS.last bs
    bs' = BS.init bs

-- | Integer decoding.
decodeOne :: Word8 -> Int
decodeOne = fromIntegral

----------------------------------------------------------------

-- |
--
-- >>> parseInteger 7 127 $ BS.pack [210,211,212,87,88,89,90]
-- (183839313,"XYZ")
parseInteger :: Int -> Word8 -> ByteString -> (Int, ByteString)
parseInteger n w bs
  | i < p     = (i, bs)
  | otherwise = (len, rest)
  where
    p = powerArray ! n
    i = fromIntegral w
    Just idx = BS.findIndex (< 128) bs
    (bs', rest) = BS.splitAt (idx + 1) bs
    len = decode n w bs'

