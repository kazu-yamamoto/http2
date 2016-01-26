{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Network.HPACK2.Huffman.Decode (
  -- * Huffman decoding
    HuffmanDecoding
  , decode
  ) where

import Control.Exception (throwIO)
import Data.Array (Array, (!), listArray)
import Data.Bits
import Data.ByteString.Internal
import Data.IORef
import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Network.HPACK2.Huffman.Bit
import Network.HPACK2.Huffman.Params
import Network.HPACK2.Huffman.Table
import Network.HPACK2.Huffman.Tree
import Network.HPACK2.Types (DecodeError(..), Buffer, BufferSize)

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

----------------------------------------------------------------

data WorkingBuffer = WorkingBuffer {
    start :: !(Ptr Word8)
  , limit :: !(Ptr Word8)
  , offset :: !(IORef (Ptr Word8))
  }

newWorkingBuffer :: Buffer -> BufferSize -> IO WorkingBuffer
newWorkingBuffer buf siz = WorkingBuffer buf (buf `plusPtr` siz) <$> newIORef buf

write :: WorkingBuffer -> Word8 -> IO ()
write WorkingBuffer{..} w = do
    ptr <- readIORef offset
    if ptr >= limit then
        throwIO TooLongHeaderString
      else do
        poke ptr w
        let ptr' = ptr `plusPtr` 1
        writeIORef offset ptr'

toByteString :: WorkingBuffer -> IO ByteString
toByteString WorkingBuffer{..} = do
    ptr <- readIORef offset
    let !len = ptr `minusPtr` start
    create len $ \p -> memcpy p start len

----------------------------------------------------------------

-- | Huffman decoding.
type HuffmanDecoding = ByteString -> Buffer -> BufferSize -> IO ByteString

----------------------------------------------------------------

data Pin = EndOfString
         | Forward {-# UNPACK #-} !Word8 -- node no.
         | GoBack  {-# UNPACK #-} !Word8 -- node no.
                   {-# UNPACK #-} !Word8 -- a decoded value
         deriving Show

data Way16 = Way16 !(Maybe Int) !(Array Word8 Pin)
type Way256 = Array Word8 Way16

next :: Way16 -> Word8 -> Pin
next (Way16 _ a16) w = a16 ! w

----------------------------------------------------------------

-- | Huffman decoding.
decode :: HuffmanDecoding
decode bs buf siz = do
    wrkbuf <- newWorkingBuffer buf siz
    nibsrc <- newNibbleSource bs
    dec nibsrc wrkbuf

dec :: NibbleSource -> WorkingBuffer -> IO ByteString
dec src tmp = go (way256 ! 0)
  where
    go way = do
        mn <- getNibble src
        case mn of
            Nothing -> case way of
                Way16 Nothing  _ -> throwIO IllegalEos
                Way16 (Just i) _
                  | i <= 8       -> toByteString tmp
                  | otherwise    -> throwIO TooLongEos
            Just w  -> case next way w of
                EndOfString      -> throwIO EosInTheMiddle
                Forward n        -> go (way256 ! n)
                GoBack  n v      -> do
                    write tmp v
                    go (way256 ! n)

----------------------------------------------------------------

way256 :: Way256
way256 = construct $ toHTree huffmanTable

construct :: HTree -> Way256
construct decoder = listArray (0,255) $ map to16ways $ flatten decoder
  where
    to16ways x = Way16 ei a16
      where
        !ei = eosInfo x
        !a16 = listArray (0,15) $ map (step decoder x Nothing) bits4s

step :: HTree -> HTree -> Maybe Word8 -> [B] -> Pin
step root (Tip _ v)     _  bss
  | v == idxEos                     = EndOfString
  | otherwise                       = let w = fromIntegral v
                                      in step root root (Just w) bss
step _    (Bin _ n _ _) Nothing  [] = Forward (fromIntegral n)
step _    (Bin _ n _ _) (Just w) [] = GoBack (fromIntegral n) w
step root (Bin _ _ l _) mx   (F:bs) = step root l mx bs
step root (Bin _ _ _ r) mx   (T:bs) = step root r mx bs

bits4s :: [[B]]
bits4s = [
    [F,F,F,F]
  , [F,F,F,T]
  , [F,F,T,F]
  , [F,F,T,T]
  , [F,T,F,F]
  , [F,T,F,T]
  , [F,T,T,F]
  , [F,T,T,T]
  , [T,F,F,F]
  , [T,F,F,T]
  , [T,F,T,F]
  , [T,F,T,T]
  , [T,T,F,F]
  , [T,T,F,T]
  , [T,T,T,F]
  , [T,T,T,T]
  ]
