module HexString (fromHexString, toHexString) where

import Data.ByteString.Internal (ByteString(..), unsafeCreate)
import Data.Word8
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peek, poke, peekByteOff, pokeByteOff)

fromHexString :: ByteString -> ByteString
fromHexString (PS fptr off len) = unsafeCreate size $ \dst ->
    withForeignPtr fptr $ \src -> go (src `plusPtr` off) dst 0
  where
    size = len `div` 2
    go from to bytes
      | bytes == size = return ()
      | otherwise    = do
          w1 <- peek from
          w2 <- peekByteOff from 1
          let w = hex2w (w1,w2)
          poke to w
          go (from `plusPtr` 2) (to `plusPtr` 1) (bytes + 1)

hex2w :: (Word8, Word8) -> Word8
hex2w (w1,w2) = h2w w1 * 16 + h2w w2

h2w :: Word8 -> Word8
h2w w
  | isDigit w = w - _0
  | otherwise = w - _a + 10

toHexString :: ByteString -> ByteString
toHexString (PS fptr off len) = unsafeCreate size $ \dst ->
    withForeignPtr fptr $ \src -> go (src `plusPtr` off) dst 0
  where
    size = len * 2
    go from to bytes
      | bytes == len = return ()
      | otherwise    = do
          w <- peek from
          let (w1,w2) = w2hex w
          poke to w1
          pokeByteOff to 1 w2
          go (from `plusPtr` 1) (to `plusPtr` 2) (bytes + 1)

w2hex :: Word8 -> (Word8, Word8)
w2hex w = (w2h w1, w2h w2)
  where
    (w1,w2) = w `divMod` 16

w2h :: Word8 -> Word8
w2h w
  | w < 10    = w + _0
  | otherwise = w + _a - 10
