{-# LANGUAGE BangPatterns, OverloadedStrings, CPP, RecordWildCards #-}

module Network.HPACK.HeaderBlock.Integer (
    encode
  , encodeInteger
  , decode
  , decodeInteger
  , calLen
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Array (Array, listArray, (!))
import Data.Bits ((.&.), shiftR, testBit)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke)
import Network.HPACK.Buffer
import Network.HPACK.Types (BufferOverrun(..))

-- $setup
-- >>> import qualified Data.ByteString as BS

powerArray :: Array Int Int
powerArray = listArray (1,8) [1,3,7,15,31,63,127,255]

----------------------------------------------------------------

calLen :: Int -> Int -> Int
calLen n i
  | i < p     = 1
  | otherwise = let !r = go (i - p) 1 in r
  where
    !p = powerArray ! n
    go !j !m
      | j < 127   = m + 1
      | otherwise = go (j `shiftR` 7) (m + 1)

{-
if I < 2^N - 1, encode I on N bits
   else
       encode (2^N - 1) on N bits
       I = I - (2^N - 1)
       while I >= 128
            encode (I % 128 + 128) on 8 bits
            I = I / 128
       encode I on 8 bits
-}

encodeInteger :: Int -> Int -> IO ByteString
encodeInteger n i = withTemporaryBuffer 4096 $ \wbuf -> encode wbuf id n i

{-# INLINABLE encode #-}
encode :: WorkingBuffer -> (Word8 -> Word8) -> Int -> Int -> IO ()
encode WorkingBuffer{..} set n i = do
      beg <- readIORef offset
      let !end = beg `plusPtr` len
      when (end >= limit) $ throwIO BufferOverrun
      if i < p then
          poke beg $ set $ fromIntegral i
        else do
          poke beg $ set $ fromIntegral p
          go (i - p) (beg `plusPtr` 1)
      writeIORef offset end
  where
    !p = powerArray ! n
    !len = calLen n i
    go :: Int -> Ptr Word8 -> IO ()
    go !j !ptr
      | j < 128   = poke ptr $ fromIntegral j
      | otherwise = do
          let !q = j `shiftR` 7
              !r = j .&. 0x7f
          poke ptr $ fromIntegral (r + 128)
          go q (ptr `plusPtr` 1)

----------------------------------------------------------------

{-
decode I from the next N bits
   if I < 2^N - 1, return I
   else
       M = 0
       repeat
           B = next octet
           I = I + (B & 127) * 2^M
           M = M + 7
       while B & 128 == 128
       return I
-}

-- | Integer decoding. The first argument is N of prefix.
--
-- >>> decodeInteger 5 10 $ BS.empty
-- 10
-- >>> decodeInteger 5 31 $ BS.pack [154,10]
-- 1337
-- >>> decodeInteger 8 42 $ BS.empty
-- 42
decodeInteger :: Int -> Word8 -> ByteString -> IO Int
decodeInteger n w bs = withReadBuffer bs $ \rbuf -> decode n w rbuf

{-# INLINABLE decode #-}
-- | Integer decoding. The first argument is N of prefix.
decode :: Int -> Word8 -> ReadBuffer -> IO Int
decode n w rbuf
  | i < p     = return i
  | otherwise = decode' 0 i
  where
    p = powerArray ! n
    i = fromIntegral w
    decode' :: Int -> Int -> IO Int
    decode' m j = do
        !b <- fromIntegral <$> getByte rbuf
        let !j' = j + (b .&. 0x7f) * 2 ^ m
            !m' = m + 7
            !cont = b `testBit` 7
        if cont then decode' m' j' else return j'
