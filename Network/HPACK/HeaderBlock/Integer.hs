module Network.HPACK.HeaderBlock.Integer (
    encode
  , encodeOne
  , decode
  , decodeOne
  ) where

import Data.Word (Word8)

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
    p = 2 ^ n - 1

encode' :: Int -> [Word8]
encode' i
  | i < 128   = fromIntegral i : []
  | otherwise = fromIntegral (r + 128) : encode' q
  where
    (q,r) = i `divMod` 128

----------------------------------------------------------------

encodeOne :: Int -> Word8
encodeOne = fromIntegral

----------------------------------------------------------------

-- | Integer decoding. The first argument is N of prefix.
--
-- >>> decode 5 [10]
-- 10
-- >>> decode 5 [31,154,10]
-- 1337
-- >>> decode 8 [42]
-- 42
decode :: Int -> [Word8] -> Int
decode = undefined

----------------------------------------------------------------

decodeOne :: Word8 -> Int
decodeOne = fromIntegral
