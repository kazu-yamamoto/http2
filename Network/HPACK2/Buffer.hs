{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Network.HPACK2.Buffer (
    WorkingBuffer
  , newWorkingBuffer
  , writeWord8
  , finalPointer
  , toByteString
  , NibbleSource
  , newNibbleSource
  , getNibble
  ) where

import Foreign.ForeignPtr (withForeignPtr)
import Data.Bits (shiftR, (.&.))
import Data.ByteString.Internal (ByteString(..), create, memcpy)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import Foreign.Storable (peek, poke)
import Network.HPACK2.Types (Buffer, BufferSize)

----------------------------------------------------------------

data WorkingBuffer = WorkingBuffer {
    start :: !(Ptr Word8)
  , limit :: !(Ptr Word8)
  , offset :: !(IORef (Ptr Word8))
  }

newWorkingBuffer :: Buffer -> BufferSize -> IO WorkingBuffer
newWorkingBuffer buf siz = WorkingBuffer buf (buf `plusPtr` siz) <$> newIORef buf

{-# INLINE writeWord8 #-}
writeWord8 :: WorkingBuffer -> Word8 -> IO Bool
writeWord8 WorkingBuffer{..} w = do
    ptr <- readIORef offset
    if ptr >= limit then
        return False
      else do
        poke ptr w
        let ptr' = ptr `plusPtr` 1
        writeIORef offset ptr'
        return True

finalPointer :: WorkingBuffer -> IO (Ptr Word8)
finalPointer WorkingBuffer{..} = readIORef offset

toByteString :: WorkingBuffer -> IO ByteString
toByteString WorkingBuffer{..} = do
    ptr <- readIORef offset
    let !len = ptr `minusPtr` start
    create len $ \p -> memcpy p start len

----------------------------------------------------------------
data Digit = Upper | Lower deriving Eq

data NibbleSource = NibbleSource {
    beg :: !(Ptr Word8)
  , end :: !(Ptr Word8)
  , cur :: !(IORef (Ptr Word8))
  , dig :: !(IORef Digit)
  }

newNibbleSource :: ByteString -> IO NibbleSource
newNibbleSource (PS fp off len) = withForeignPtr fp $ \ptr -> do
    let !bg = ptr `plusPtr` off
        !ed = bg `plusPtr` len
    NibbleSource bg ed <$> newIORef bg <*> newIORef Upper

{-# INLINE getNibble #-}
getNibble :: NibbleSource -> IO (Maybe Word8)
getNibble NibbleSource{..} = do
    ptr <- readIORef cur
    if ptr >= end then
        return Nothing
      else do
        d <- readIORef dig
        w <- peek ptr
        if d == Upper then do
            writeIORef dig Lower
            let !nib = w `shiftR` 4
            return $! Just nib
         else do
            writeIORef dig Upper
            writeIORef cur $ ptr `plusPtr` 1
            let !nib = w .&. 0x0f
            return $! Just nib
