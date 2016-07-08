{-# LANGUAGE BangPatterns, CPP, RecordWildCards #-}

module Network.HPACK.Huffman.Encode (
  -- * Huffman encoding
    HuffmanEncoding
  , encode
  , encodeHuffman
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Monad (void)
import Data.Array
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Foreign.Ptr (plusPtr, minusPtr, Ptr)
import Foreign.Storable (peek, poke)
import Network.HPACK.Buffer
import Network.HPACK.Huffman.Bit
import Network.HPACK.Huffman.Params
import Network.HPACK.Huffman.Table
import Data.IORef

----------------------------------------------------------------

type AOSA = Array Int ShiftedArray

type ShiftedArray = Array Int Shifted

data WS = W0
        | W1 !Word8
        | W2 !Word8 !Word8
        | W3 !Word8 !Word8 !Word8
        | W4 !Word8 !Word8 !Word8 !Word8
        deriving Show

data Shifted = Shifted !Int   -- How many bits in the last byte
                       !Int   -- Total bytes (3rd + 4th)
                       !Word8 -- First word. If Int is 0, this is dummy
                       !WS    -- Following words, up to 4 bytes
                       deriving Show

----------------------------------------------------------------

aosa :: AOSA
aosa = listArray (0,idxEos) $ map toShiftedArray huffmanTable

-- |
--
-- >>> toShifted [T,T,T,T] 0
-- Shifted 4 1 240 W0
-- >>> toShifted [T,T,T,T] 4
-- Shifted 0 1 15 W0
-- >>> toShifted [T,T,T,T] 5
-- Shifted 1 2 7 (W1 128)

toShifted :: Bits -> Int -> Shifted
toShifted bits n = Shifted r siz w ws
  where
    shifted = replicate n F ++ bits
    len = length shifted
    !r = len `mod` 8
    ws0 = map fromBits $ group8 shifted
    !siz = length ws0
    !w = head ws0
    !ws = case tail ws0 of
        []            -> W0
        [w1]          -> W1 w1
        [w1,w2]       -> W2 w1 w2
        [w1,w2,w3]    -> W3 w1 w2 w3
        [w1,w2,w3,w4] -> W4 w1 w2 w3 w4
        _             -> error "toShifted"
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
enc WorkingBuffer{..} rbuf = do
    beg <- readIORef offset
    end <- go 0 beg
    writeIORef offset end
    let !len = end `minusPtr` beg
    return len
  where
    go n ptr = do
        more <- hasOneByte rbuf
        if more then do
            !i <- fromIntegral <$> getByte rbuf
            let Shifted n' len b bs = (aosa ! i) ! n
            if n == 0 then
                poke ptr b
              else do
                b0 <- peek ptr
                poke ptr (b0 .|. b)
            copy (ptr `plusPtr` 1) bs
            let !ptr' | n' == 0   = ptr `plusPtr` len
                      | otherwise = ptr `plusPtr` (len - 1)
            go n' ptr'
          else
            if (n == 0) then
                return ptr
              else do
                let Shifted _ _ b _ = (aosa ! idxEos) ! n
                b0 <- peek ptr
                poke ptr (b0 .|. b)
                let !ptr' = ptr `plusPtr` 1
                return ptr'

{-# INLINE copy #-}
copy :: Ptr Word8 -> WS -> IO ()
copy _ W0 = return ()
copy ptr (W1 w1) = poke ptr w1
copy ptr (W2 w1 w2) = do
    poke ptr w1
    poke (ptr `plusPtr` 1) w2
copy ptr (W3 w1 w2 w3) = do
    poke ptr w1
    poke (ptr `plusPtr` 1) w2
    poke (ptr `plusPtr` 2) w3
copy ptr (W4 w1 w2 w3 w4) = do
    poke ptr w1
    poke (ptr `plusPtr` 1) w2
    poke (ptr `plusPtr` 2) w3
    poke (ptr `plusPtr` 3) w4

encodeHuffman :: ByteString -> IO ByteString
encodeHuffman bs = withTemporaryBuffer 4096 $ \wbuf ->
    void $ encode wbuf bs
