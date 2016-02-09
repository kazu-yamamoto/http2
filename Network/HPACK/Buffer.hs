{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Network.HPACK.Buffer (
    Buffer
  , BufferSize
  , WorkingBuffer
  , newWorkingBuffer
  , wind
  , readWord8
  , writeWord8
  , shiftLastN
  , returnLength
  , toByteString
  , copyByteString
  , withTemporaryBuffer
  , ReadBuffer
  , withReadBuffer
  , hasOneByte
  , hasMoreBytes
  , rewindOneByte
  , getByte
  , extractByteString
  ) where

import Control.Exception (bracket, throwIO)
import Data.ByteString.Internal (ByteString(..), create, memcpy)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc
import Foreign.Ptr (plusPtr, minusPtr)
import Foreign.Storable (peek, poke)
import Network.HPACK.Types (Buffer, BufferSize, BufferOverrun(..))

----------------------------------------------------------------

data WorkingBuffer = WorkingBuffer {
    start :: !Buffer
  , limit :: !Buffer
  , offset :: !(IORef Buffer)
  }

newWorkingBuffer :: Buffer -> BufferSize -> IO WorkingBuffer
newWorkingBuffer buf siz = WorkingBuffer buf (buf `plusPtr` siz) <$> newIORef buf

{-# INLINE wind #-}
wind :: WorkingBuffer -> Int -> IO ()
wind WorkingBuffer{..} n = do
    ptr <- readIORef offset
    let !ptr' = ptr `plusPtr` n
    writeIORef offset ptr'

{-# INLINE readWord8 #-}
readWord8 :: WorkingBuffer -> IO Word8
readWord8 WorkingBuffer{..} = readIORef offset >>= peek

{-# INLINE writeWord8 #-}
writeWord8 :: WorkingBuffer -> Word8 -> IO ()
writeWord8 WorkingBuffer{..} w = do
    ptr <- readIORef offset
    if ptr >= limit then
        throwIO BufferOverrun
      else do
        poke ptr w
        let !ptr' = ptr `plusPtr` 1
        writeIORef offset ptr'

{-# INLINE shiftLastN #-}
shiftLastN :: WorkingBuffer -> Int -> Int -> IO ()
shiftLastN WorkingBuffer{..} i len = do
    ptr <- readIORef offset
    let !src = ptr `plusPtr` negate len
        !dst = src `plusPtr` i
        !ptr' = ptr `plusPtr` i
    if ptr' >= limit then
        throwIO BufferOverrun
      else do
        memcpy dst src len
        writeIORef offset ptr'

{-# INLINE copyByteString #-}
copyByteString :: WorkingBuffer -> ByteString -> IO ()
copyByteString WorkingBuffer{..} (PS fptr off len) = withForeignPtr fptr $ \ptr -> do
    let src = ptr `plusPtr` off
    dst <- readIORef offset
    let !dst' = dst `plusPtr` len
    if dst' >= limit then
        throwIO BufferOverrun
      else do
        memcpy dst src len
        writeIORef offset dst'

toByteString :: WorkingBuffer -> IO ByteString
toByteString WorkingBuffer{..} = do
    ptr <- readIORef offset
    let !len = ptr `minusPtr` start
    create len $ \p -> memcpy p start len

{-# INLINE returnLength #-}
returnLength :: WorkingBuffer -> IO () -> IO Int
returnLength WorkingBuffer{..} body = do
    beg <- readIORef offset
    body
    end <- readIORef offset
    return $ end `minusPtr` beg

withTemporaryBuffer :: Int -> (WorkingBuffer -> IO ()) -> IO ByteString
withTemporaryBuffer siz action = bracket (mallocBytes siz) free $ \buf -> do
    wbuf <- newWorkingBuffer buf 4096
    action wbuf
    toByteString wbuf

----------------------------------------------------------------

data ReadBuffer = ReadBuffer {
    beg :: !Buffer
  , end :: !Buffer
  , cur :: !(IORef Buffer)
  }

withReadBuffer :: ByteString -> (ReadBuffer -> IO a) -> IO a
withReadBuffer (PS fp off len) action = withForeignPtr fp $ \ptr -> do
    let !bg = ptr `plusPtr` off
        !ed = bg `plusPtr` len
    nsrc <- ReadBuffer bg ed <$> newIORef bg
    action nsrc

{-# INLINE hasOneByte #-}
hasOneByte :: ReadBuffer -> IO Bool
hasOneByte ReadBuffer{..} = do
    ptr <- readIORef cur
    return $! ptr < end

{-# INLINE hasMoreBytes #-}
hasMoreBytes :: ReadBuffer -> Int -> IO Bool
hasMoreBytes ReadBuffer{..} n = do
    ptr <- readIORef cur
    return $! (end `minusPtr` ptr) >= n

{-# INLINE rewindOneByte #-}
rewindOneByte :: ReadBuffer -> IO ()
rewindOneByte ReadBuffer{..} = modifyIORef' cur (`plusPtr` (-1))

{-# INLINE getByte #-}
getByte :: ReadBuffer -> IO Word8
getByte ReadBuffer{..} = do
    ptr <- readIORef cur
    w <- peek ptr
    writeIORef cur $! ptr `plusPtr` 1
    return w

extractByteString :: ReadBuffer -> Int -> IO ByteString
extractByteString ReadBuffer{..} len = do
    src <- readIORef cur
    bs <- create len $ \dst -> memcpy dst src len
    writeIORef cur $! src `plusPtr` len
    return bs
