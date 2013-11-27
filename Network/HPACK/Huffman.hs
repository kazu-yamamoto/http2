module Network.HPACK.Huffman (
    encode
  , toEncoder
  , Encoder
  , decode
  , toDecoder
  , Decoder
  ) where

import Control.Arrow (second)
import Data.Array (Array, (!), listArray)
import Network.HPACK.Bit

----------------------------------------------------------------

newtype Encoder = Encoder (Array Int Bits)

idxEos :: Int
idxEos = 256

enc :: Encoder -> Int -> Bits
enc (Encoder ary) i = ary ! i

toEncoder :: [Bits] -> Encoder
toEncoder bs = Encoder $ listArray (0,256) bs

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

data Decoder = Tip Int | Bin Decoder Decoder deriving Show

dec :: Decoder -> Bits -> (Int,Bits)
dec (Tip i)   xs     = (i,xs)
dec (Bin l _) (F:xs) = dec l xs
dec (Bin _ r) (T:xs) = dec r xs
dec _         []     = (-1,[])

toDecoder :: [Bits] -> Decoder
toDecoder decoder = build $ zip [0..idxEos] decoder

build :: [(Int,Bits)] -> Decoder
build [(i,[])]    = Tip i
build xs = Bin (build fs) (build ts)
  where
    fs = map (second tail) $ filter ((==) F . head . snd) xs
    ts = map (second tail) $ filter ((==) T . head . snd) xs

decode :: Decoder -> [Int] -> [Int]
decode decoder is = decodeBits decoder (concatMap toBits is)

decodeBits :: Decoder -> Bits -> [Int]
decodeBits _       [] = []
decodeBits decoder xs
  | i < 0             = []
  | otherwise         = i : decodeBits decoder ys
  where
    (i,ys) = dec decoder xs
