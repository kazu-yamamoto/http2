{-# LANGUAGE BangPatterns #-}

module Network.HPACK.Bit (
    B(..)
  , Bits
  , toInt
  , toBits
  ) where

import Data.List (foldl')

data B = F | T deriving (Eq,Ord,Enum,Show)

type Bits = [B]

toInt :: Bits -> Int
toInt = foldl' (\x y -> x * 2 + y) 0 . map fromEnum

toBits :: Int -> Bits
toBits = toBits' [] 0

toBits' :: Bits -> Int -> Int -> Bits
toBits' bs !cnt 0
  | cnt == 8 = bs
  | otherwise = replicate (8 - cnt) F ++ bs -- FIXME: performance
toBits' bs !cnt x = toBits' (b:bs) (cnt + 1) q
  where
    q = x `div` 2
    r = x `mod` 2
    b = if r == 0 then F else T
