{-# LANGUAGE BangPatterns #-}

module Network.HPACK.Huffman.Encode (
  -- * Huffman encoding
    HuffmanEncoding
  , encode
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Array
import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString(..), create)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek, poke)
import Network.HPACK.Huffman.Bit
import Network.HPACK.Huffman.ByteString
import Network.HPACK.Huffman.Params
import Network.HPACK.Huffman.Table
import System.IO.Unsafe (unsafePerformIO)

----------------------------------------------------------------

type AOSA = Array Int ShiftedArray

type ShiftedArray = Array Int Shifted

data Shifted = Shifted !Int  -- Total bytes
                       !Int  -- How many bits in the last byte
                       ByteString -- Up to 5 bytes
                       deriving Show

----------------------------------------------------------------

aosa :: AOSA
aosa = listArray (0,idxEos) $ map toShiftedArray huffmanTable

-- fixme
-- |
--
-- >>> toShifted [T,T,T,T] 0
-- Shifted 1 4 "\240"
-- >>> toShifted [T,T,T,T] 4
-- Shifted 1 0 "\SI"
-- >>> toShifted [T,T,T,T] 5
-- Shifted 2 1 "\a\128"

toShifted :: Bits -> Int -> Shifted
toShifted bits n = Shifted total r bs
  where
    shifted = replicate n F ++ bits
    len = length shifted
    (q,r) = len `divMod` 8
    total
      | r == 0    = q
      | otherwise = q + 1
    bs = BS.pack $ map fromBits $ group8 shifted
    group8 xs
      | null zs   = pad ys : []
      | otherwise = ys : group8 zs
      where
        (ys,zs) = splitAt 8 xs
    pad xs = take 8 $ xs ++ repeat F


toShiftedArray :: Bits -> ShiftedArray
toShiftedArray bits = listArray (0,7) $ map (toShifted bits) [0..7]

----------------------------------------------------------------

-- | Huffman encoding.
type HuffmanEncoding = ByteString -> ByteString

-- | Huffman encoding.
encode :: HuffmanEncoding
encode (PS fptr off len) = unsafePerformIO $ withForeignPtr fptr $ \ptr -> do
    let beg = ptr `plusPtr` off
        end = beg `plusPtr` len
    size <- accumSize beg end 0 0
    create size (\dst -> go dst 0 beg end)
  where
    accumSize :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> IO Int
    accumSize src lim n acc
      | src == lim = return acc
      | otherwise  = do
          i <- fromIntegral <$> peek src
          let Shifted l n' _ = (aosa ! i) ! n
          let !acc'
               | n == 0    = acc + l
               | otherwise = acc + l - 1
          accumSize (src `plusPtr` 1) lim n' acc'
    go :: Ptr Word8 -> Int -> Ptr Word8 -> Ptr Word8 -> IO ()
    go dst n src lim
      | src == lim = do
          when (n /= 0) $ do
              let Shifted _ _ bs = (aosa ! idxEos) ! n
              w0 <- peek dst
              let w1 = BS.head bs
              poke dst (w0 .|. w1)
      | otherwise  = do
          i <- fromIntegral <$> peek src
          let Shifted l n' bs = (aosa ! i) ! n
          if n == 0 then
              copy dst bs
            else do
              w0 <- peek dst
              copy dst bs
              w1 <- peek dst
              poke dst (w0 .|. w1)
          let dst'
               | n' == 0   = dst `plusPtr` l
               | otherwise = dst `plusPtr` (l - 1)
          go dst' n' (src `plusPtr` 1) lim
