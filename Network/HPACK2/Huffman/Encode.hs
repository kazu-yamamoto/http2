{-# LANGUAGE BangPatterns, CPP #-}

module Network.HPACK2.Huffman.Encode (
  -- * Huffman encoding
    HuffmanEncoding
  , encode
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Monad (when)
import Data.Array
import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word8)
import Foreign.Ptr (minusPtr)
import Network.HPACK2.Buffer
import Network.HPACK2.Huffman.Bit
import Network.HPACK2.Huffman.Params
import Network.HPACK2.Huffman.Table

----------------------------------------------------------------

type AOSA = Array Int ShiftedArray

type ShiftedArray = Array Int Shifted

data Shifted = Shifted !Int        -- How many bits in the last byte
                       !Word8      -- First word. If Int is 0, this is dummy
                       !ByteString -- Following words, up to 4 bytes
                       deriving Show

----------------------------------------------------------------

aosa :: AOSA
aosa = listArray (0,idxEos) $ map toShiftedArray huffmanTable

-- |
--
-- >>> toShifted [T,T,T,T] 0
-- Shifted 1 4 "\240"
-- >>> toShifted [T,T,T,T] 4
-- Shifted 1 0 "\SI"
-- >>> toShifted [T,T,T,T] 5
-- Shifted 2 1 "\a\128"

toShifted :: Bits -> Int -> Shifted
toShifted bits n = Shifted r w bs
  where
    shifted = replicate n F ++ bits
    len = length shifted
    !r = len `mod` 8
    bs0 = BS.pack $ map fromBits $ group8 shifted
    (!w,!bs) = (BS.head bs0, BS.tail bs0)
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
type HuffmanEncoding = WorkingBuffer -> ByteString -> IO Int

-- | Huffman encoding.
encode :: HuffmanEncoding
encode dst bs = withReadBuffer bs $ enc dst

enc :: WorkingBuffer -> ReadBuffer -> IO Int
enc dst rbuf = do
    beg <- currentOffset dst
    go 0
    end <- currentOffset dst
    return $ end `minusPtr` beg
  where
    go n = do
        more <- hasOneByte rbuf
        if more then do
            !i <- fromIntegral <$> getByte rbuf
            let Shifted n' b bs = (aosa ! i) ! n
            if n == 0 then do
                writeWord8 dst b
                copyByteString dst bs
              else do
                b0 <- readWord8 dst
                writeWord8 dst (b0 .|. b)
                copyByteString dst bs
            when (n' /= 0) $ wind dst (-1)
            go n'
          else do
            when (n /= 0) $ do
                let Shifted _ b _ = (aosa ! idxEos) ! n
                b0 <- readWord8 dst
                writeWord8 dst (b0 .|. b)
