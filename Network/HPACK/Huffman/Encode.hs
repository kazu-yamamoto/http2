{-# LANGUAGE BangPatterns #-}

module Network.HPACK.Huffman.Encode (
  -- * Huffman encoding
    Encoder
  , toEncoder
  , HuffmanEncoding
  , encode
  ) where

import Control.Applicative ((<$>))
import Data.Array (Array, (!), listArray)
import Data.Bits ((.&.), shiftR)
import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek)
import Network.HPACK.Builder
import Network.HPACK.Huffman.Bit
import System.IO.Unsafe (unsafePerformIO)

----------------------------------------------------------------

-- | Huffman encoding.
type HuffmanEncoding = ByteString -> ByteString

----------------------------------------------------------------

idxEos :: Int
idxEos = 256

----------------------------------------------------------------

-- | Type for Huffman encoding.
newtype Encoder = Encoder (Array Int (Int,Bits))

-- | Creating 'Encoder'.
toEncoder :: [Bits] -> Encoder
toEncoder bss = Encoder $ listArray (0,idxEos) (map toEnt bss)
  where
    toEnt bs = (len, bs)
      where
        !len = length bs

-- | Huffman encoding.
encode :: Encoder -> HuffmanEncoding
encode encoder (PS fptr off len) = fromBitsToByteString nBytes (run b)
  where
    (nbits,b0) = unsafePerformIO $ withForeignPtr fptr $ \ptr ->
        loop (ptr `plusPtr` off) 0 0 empty
    (nBytes,b) = eos nbits b0
    loop :: Ptr Word8 -> Int -> Int -> Builder Bits -> IO (Int,Builder Bits)
    loop !ptr !cnt !nBits !builder
      | cnt == len = return (nBits,builder)
      | otherwise  = do
          i <- fromIntegral <$> peek ptr
          let (bits,bs) = enc encoder i
              builder' = builder << bs
          loop (ptr `plusPtr` 1) (cnt + 1) (nBits + bits) builder'
    eos nBits !builder
      | r == 0    = (q,builder)
      | otherwise = (q+1, builder << bools')
      where
--        (q,r) = nBits `divMod` 8
        q = nBits `shiftR` 3
        r = nBits .&. 0x7
        (_,bools) = enc encoder idxEos
        bools' = take (8 - r) bools

enc :: Encoder -> Int -> (Int,Bits)
enc (Encoder ary) i = ary ! i
