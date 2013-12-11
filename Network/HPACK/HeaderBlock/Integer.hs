module Network.HPACK.HeaderBlock.Integer where

import Data.Word (Word8)

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

-- | Integer decoding. The first argument is N of prefix.
decode :: Int -> [Word8] -> Int
decode = undefined
