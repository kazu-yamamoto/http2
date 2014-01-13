{-# LANGUAGE BangPatterns #-}

module Network.HPACK.Huffman.Bit (
  -- * Bits
    B(..)
  , Bits
  , fromBitsToByteString
  -- * Bit source
  , BitSource
  , toBitSource
  , uncons
  ) where

import Data.Array
import Data.Bits (setBit, testBit)
import Data.ByteString.Internal (ByteString, unsafeCreate)
import qualified Data.ByteString as BS
import Foreign.Ptr
import Foreign.Storable (poke)
import Data.Word (Word8)

-- | Data type for Bit.
data B = F -- ^ Zero
       | T -- ^ One
       deriving (Eq,Ord,Show)

-- | Bit stream.
type Bits = [B]

-- | Converting '[Bits]' to 'ByteString'.
--   The first arguments is the length in bytes.
fromBitsToByteString :: Int -> [Bits] -> ByteString
fromBitsToByteString len bss = unsafeCreate len $ loop [] bss
  where
    loop [] [] _       = return ()
    loop [] (b:bs) ptr = do
        (ptr', rest') <- write ptr b
        loop rest' bs ptr'
    loop rest (b:bs) ptr = do
        (ptr', rest') <- write ptr (rest ++ b) -- inefficient?
        loop rest' bs ptr'
    loop _ _ _      = error "loop"
    write ptr (b7:b6:b5:b4:b3:b2:b1:b0:bs) = do
        poke ptr (toWord b7 b6 b5 b4 b3 b2 b1 b0)
        write (ptr `plusPtr` 1) bs
    write ptr bs = return (ptr,bs)
    set w _ F = w
    set w i T = w `setBit` i
    toWord b7 b6 b5 b4 b3 b2 b1 b0 = w7
      where
        w0 = set  0 0 b0
        w1 = set w0 1 b1
        w2 = set w1 2 b2
        w3 = set w2 3 b3
        w4 = set w3 4 b4
        w5 = set w4 5 b5
        w6 = set w5 6 b6
        w7 = set w6 7 b7

-- | Source for bit stream.
data BitSource = BitSource [B] !ByteString

-- | Converting 'ByteString' to 'BitSource'
toBitSource :: ByteString -> BitSource
toBitSource bs = BitSource [] bs

-- | Extracting a bit from 'BitSource'.
uncons :: BitSource -> Maybe (B,BitSource)
uncons (BitSource [] bs) = case BS.uncons bs of
    Nothing      -> Nothing
    Just (w,bs') -> let x:xs = bitArray ! w
                    in Just $ (x, BitSource xs bs')
uncons (BitSource (x:xs) bs) = Just (x, BitSource xs bs)

bitArray :: Array Word8 Bits
bitArray = listArray (0,255) $ map toBits [0..255]

-- | From 'Word8' to 'Bits' of length 8.
--
-- >>> toBits 170
-- [T,F,T,F,T,F,T,F]
-- >>> toBits 85
-- [F,T,F,T,F,T,F,T]
toBits :: Word8 -> Bits
toBits w = [b7,b6,b5,b4,b3,b2,b1,b0]
  where
    get i
      | w `testBit` i = T
      | otherwise     = F
    b0 = get 0
    b1 = get 1
    b2 = get 2
    b3 = get 3
    b4 = get 4
    b5 = get 5
    b6 = get 6
    b7 = get 7
