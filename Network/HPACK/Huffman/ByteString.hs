{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.HPACK.Huffman.ByteString (
    unpack4bits
  , copy
  ) where

import Foreign.C.Types (CSize(..))
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Imports

-- $setup
-- >>> import qualified Data.ByteString as BS

-- |
--
-- >>> let bs = BS.pack [0x12,0x34,0xf3,0xab]
-- >>> unpack4bits bs
-- [1,2,3,4,15,3,10,11]
-- >>> unpack4bits $ BS.tail bs
-- [3,4,15,3,10,11]

unpack4bits :: ByteString -> [Word8]
unpack4bits = withBS $ \fptr off len ->unsafeDupablePerformIO $
  withForeignPtr fptr $ \ptr -> do
    let lim = ptr `plusPtr` (off - 1)
        end = ptr `plusPtr` (off + len - 1)
    go lim end []
  where
    go lim p ws
      | lim == p = return ws
      | otherwise = do
          w <- peek p
          let w0 = w `shiftR` 4
              w1 = w .&. 0xf
          go lim (p `plusPtr` (-1)) (w0:w1:ws)


copy :: Ptr Word8 -> ByteString -> IO ()
copy dst = withBS $ \fptr off len -> withForeignPtr fptr $ \ptr -> do
    let beg = ptr `plusPtr` off
    memcpy dst beg (fromIntegral len)

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

memcpy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpy dst src s = void $ c_memcpy dst src (fromIntegral s)

----------------------------------------------------------------
-- Compatibility helpers for bytestring

withBS :: (ForeignPtr Word8 -> Int -> Int -> a) -> ByteString -> a
#if MIN_VERSION_bytestring(0,11,0)
withBS f (BS fptr len) = f fptr 0 len
#else
withBS f (PS fptr off len) = f fptr off len
#endif
