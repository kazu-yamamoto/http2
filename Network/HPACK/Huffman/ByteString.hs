module Network.HPACK.Huffman.ByteString where

import Data.Bits ((.&.), shiftR)
import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peek)
import GHC.IO (unsafeDupablePerformIO)

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
unpack4bits (PS fptr off len) = unsafeDupablePerformIO $
  withForeignPtr fptr $ \ptr -> do
    let beg = ptr `plusPtr` off
        end = beg `plusPtr` (len - 1)
    go beg end []
  where
    go beg p ws
      | beg > p = return ws
      | otherwise = do
          w <- peek p
          let w0 = w `shiftR` 4
              w1 = w .&. 0xf
          go beg (p `plusPtr` (-1)) (w0:w1:ws)

packLoL :: Int -> [[Word8]] -> ByteString
packLoL = undefined
