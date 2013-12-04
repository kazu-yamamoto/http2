module Network.HPACK.Huffman (
  -- * Huffman encoding
    Encoder
  , toEncoder
  , encode
  -- * Huffman decoding
  , Decoder
  , toDecoder
  , decode
  ) where

import Control.Arrow (second)
import Data.Array (Array, (!), listArray)
import Data.List (partition)
import Network.HPACK.Huffman.Bit

----------------------------------------------------------------

-- | Type for Huffman encoding.
newtype Encoder = Encoder (Array Int Bits)

idxEos :: Int
idxEos = 256

enc :: Encoder -> Int -> Bits
enc (Encoder ary) i = ary ! i

-- | Creating 'Encoder'.
toEncoder :: [Bits] -> Encoder
toEncoder bs = Encoder $ listArray (0,256) bs

-- | Huffman encoding.
encode :: Encoder -> [Int] -> [Int]
encode encoder is = map toInt $ group8 bits
  where
    bits = concatMap (enc encoder) is
    group8 xs
      | null zs   = eos ys : []
      | otherwise = ys : group8 zs
      where
        (ys,zs) = splitAt 8 xs
    eos xs
      | length xs == 8 = xs
      | otherwise      = take 8 (xs ++ enc encoder idxEos)

----------------------------------------------------------------

-- | Type for Huffman decoding.
data Decoder = Tip Int | Bin Decoder Decoder deriving Show

dec :: Decoder -> Bits -> (Int,Bits)
dec (Tip i)   xs     = (i,xs)
dec (Bin l _) (F:xs) = dec l xs
dec (Bin _ r) (T:xs) = dec r xs
dec _         []     = (-1,[])

-- | Creating 'Decoder'.
toDecoder :: [Bits] -> Decoder
toDecoder decoder = build $ zip [0..idxEos] decoder

build :: [(Int,Bits)] -> Decoder
build [(i,[])]    = Tip i
build xs = Bin (build fs) (build ts)
  where
    (fs',ts') = partition ((==) F . head . snd) xs
    fs = map (second tail) fs'
    ts = map (second tail) ts'

-- | Huffman decoding.
decode :: Decoder -> [Int] -> [Int]
decode decoder is = decodeBits decoder (concatMap toBits is)

decodeBits :: Decoder -> Bits -> [Int]
decodeBits _       [] = []
decodeBits decoder xs
  | i < 0             = []
  | otherwise         = i : decodeBits decoder ys
  where
    (i,ys) = dec decoder xs
