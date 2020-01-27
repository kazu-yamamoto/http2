{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Network.HPACK.Huffman.Encode (
  -- * Huffman encoding
    encodeH
  , encodeHuffman
  ) where

import Control.Exception (throwIO)
import Data.Array.Base (unsafeAt)
import Data.Array.IArray (listArray)
import Data.Array.Unboxed (UArray)
import Data.IORef
import Foreign.Ptr (plusPtr, minusPtr)
import Foreign.Storable (poke)
import Network.ByteOrder hiding (copy)

import Imports
import Network.HPACK.Huffman.Params (idxEos)
import Network.HPACK.Huffman.Table

----------------------------------------------------------------

huffmanLength :: UArray Int Int
huffmanLength = listArray (0,idxEos) $ map length huffmanTable

huffmanCode :: UArray Int Word64
huffmanCode = listArray (0,idxEos) huffmanTable'

----------------------------------------------------------------

-- | Huffman encoding.
encodeH :: WriteBuffer
        -> ByteString -- ^ Target
        -> IO Int     -- ^ The length of the encoded string.
encodeH dst bs = withReadBuffer bs $ enc dst

-- The maximum length of Huffman code is 30.
-- 40 is enough as a work space.
initialOffset :: Int
initialOffset = 40

shiftForWrite :: Int
shiftForWrite = 32

enc :: WriteBuffer -> ReadBuffer -> IO Int
enc WriteBuffer{..} rbuf = do
    beg <- readIORef offset
    end <- go (beg,0,initialOffset)
    writeIORef offset end
    let !len = end `minusPtr` beg
    return len
  where
    go (dst,encoded,off) = do
        !i <- readInt8 rbuf
        if i >= 0 then
            cpy dst (bond i) >>= go
          else if off == initialOffset then
            return dst
          else do
            let (encoded1,_) = bond idxEos
            write dst encoded1
      where
        {-# INLINE bond #-}
        bond i = (encoded', off')
          where
            !len = huffmanLength `unsafeAt` i
            !code = huffmanCode `unsafeAt` i
            !scode = code `shiftL` (off - len)
            !encoded' = encoded .|. scode
            !off' = off - len
        {-# INLINE write #-}
        write p w = do
            when (p >= limit) $ throwIO BufferOverrun
            let !w8 = fromIntegral (w `shiftR` shiftForWrite) :: Word8
            poke p w8
            let !p' = p `plusPtr` 1
            return p'
        {-# INLINE cpy #-}
        cpy p (w,o)
          | o > shiftForWrite = return (p,w,o)
          | otherwise = do
              p' <- write p w
              let !w' = w `shiftL` 8
                  !o' = o + 8
              cpy p' (w',o')

-- | Huffman encoding with a temporary buffer whose size is 4096.
encodeHuffman :: ByteString -> IO ByteString
encodeHuffman bs = withWriteBuffer 4096 $ \wbuf ->
    void $ encodeH wbuf bs
