{-# LANGUAGE BangPatterns #-}

module Network.HPACK.Huffman.Bit (
    B(..)
  , Bits
  , fromBits
  , toBits
  ) where

import Data.List (foldl')
import Data.Word (Word8)

-- | Data type for Bit.
data B = F -- ^ Zero
       | T -- ^ One
       deriving (Eq,Ord,Show)

fromBit :: B -> Word8
fromBit F = 0
fromBit T = 1

toBit :: Word8 -> B
toBit 0 = F
toBit 1 = T
toBit _ = error "toBit"

-- | Bit sequence.
type Bits = [B]

-- | From 'Bits' of length 8 to 'Word8'.
--
-- >>> fromBits [T,F,T,F,T,F,T,F]
-- 170
-- >>> fromBits [F,T,F,T,F,T,F,T]
-- 85
fromBits :: Bits -> Word8
fromBits = foldl' (\x y -> x * 2 + y) 0 . map fromBit

-- | From 'Word8' to 'Bits' of length 8.
--
-- >>> toBits 170
-- [T,F,T,F,T,F,T,F]
-- >>> toBits 85
-- [F,T,F,T,F,T,F,T]
toBits :: Word8 -> Bits
toBits = toBits' [] 0

toBits' :: Bits -> Int -> Word8 -> Bits
toBits' bs !cnt 0
  | cnt == 8      = bs
  | otherwise     = replicate (8 - cnt) F ++ bs -- filling missing bits from MSB
toBits' bs !cnt x = toBits' (b:bs) (cnt + 1) q
  where
    (q,r) = x `divMod` 2
    b = toBit r
