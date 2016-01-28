{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Network.HPACK2.Buffer (
    Buffer
  , BufferSize
  , WorkingBuffer
  , newWorkingBuffer
  , rewind1
  , readW
  , readWord8
  , writeWord8
  , finalPointer
  , toByteString
  , copyByteString
  , NibbleSource
  , withNibbleSource
  , hasMoreNibble
  , getNibble
  ) where

import Foreign.ForeignPtr (withForeignPtr)
import Data.Bits (shiftR, (.&.))
import Data.ByteString.Internal (ByteString(..), create, memcpy)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8)
import Foreign.Ptr (plusPtr, minusPtr)
import Foreign.Storable (peek, poke)
import Network.HPACK2.Types (Buffer, BufferSize)

----------------------------------------------------------------

data WorkingBuffer = WorkingBuffer {
    start :: !Buffer
  , limit :: !Buffer
  , offset :: !(IORef Buffer)
  }

newWorkingBuffer :: Buffer -> BufferSize -> IO WorkingBuffer
newWorkingBuffer buf siz = WorkingBuffer buf (buf `plusPtr` siz) <$> newIORef buf

{-# INLINE rewind1 #-}
rewind1 :: WorkingBuffer -> IO ()
rewind1 WorkingBuffer{..} = do
    ptr <- readIORef offset
    let !ptr' = ptr `plusPtr` (-1)
    writeIORef offset ptr'

{-# INLINE readW #-}
readW :: WorkingBuffer -> IO Word8
readW WorkingBuffer{..} = readIORef offset >>= peek

{-# INLINE readWord8 #-}
readWord8 :: WorkingBuffer -> IO (Maybe Word8)
readWord8 WorkingBuffer{..} = do
    ptr <- readIORef offset
    if ptr >= limit then
        return Nothing
      else do
        w <- peek ptr
        return $! Just w

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

finalPointer :: WorkingBuffer -> IO Buffer
finalPointer WorkingBuffer{..} = readIORef offset

{-# INLINE copyByteString #-}
copyByteString :: WorkingBuffer -> ByteString -> IO Bool
copyByteString WorkingBuffer{..} (PS fptr off len) = withForeignPtr fptr $ \ptr -> do
    let src = ptr `plusPtr` off
    dst <- readIORef offset
    let !dst' = dst `plusPtr` len
    if dst' >= limit then
        return False
      else do
        memcpy dst src len
        writeIORef offset dst'
        return True

toByteString :: WorkingBuffer -> IO ByteString
toByteString WorkingBuffer{..} = do
    ptr <- readIORef offset
    let !len = ptr `minusPtr` start
    create len $ \p -> memcpy p start len

----------------------------------------------------------------
data Digit = Upper | Lower deriving Eq

data NibbleSource = NibbleSource {
    beg :: !Buffer
  , end :: !Buffer
  , cur :: !(IORef Buffer)
  , dig :: !(IORef Digit)
  }

withNibbleSource :: ByteString -> (NibbleSource -> IO a) -> IO a
withNibbleSource (PS fp off len) action = withForeignPtr fp $ \ptr -> do
    let !bg = ptr `plusPtr` off
        !ed = bg `plusPtr` len
    nsrc <- NibbleSource bg ed <$> newIORef bg <*> newIORef Upper
    action nsrc

{-# INLINE hasMoreNibble #-}
hasMoreNibble :: NibbleSource -> IO Bool
hasMoreNibble NibbleSource{..} = do
    ptr <- readIORef cur
    return $! ptr < end

{-# INLINE getNibble #-}
getNibble :: NibbleSource -> IO Word8
getNibble NibbleSource{..} = do
    ptr <- readIORef cur
    d <- readIORef dig
    w <- peek ptr
    if d == Upper then do
        writeIORef dig Lower
        let !nib = w `shiftR` 4
        return nib
     else do
        writeIORef dig Upper
        writeIORef cur $! ptr `plusPtr` 1
        let !nib = w .&. 0x0f
        return nib
