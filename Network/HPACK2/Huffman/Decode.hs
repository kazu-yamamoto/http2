{-# LANGUAGE BangPatterns, RecordWildCards, OverloadedStrings #-}

module Network.HPACK2.Huffman.Decode (
  -- * Huffman decoding
    HuffmanDecoding
  , decode
  , decodeDummy
  ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Array (Array, (!), listArray)
import Data.Bits (shiftR, (.&.))
import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word8)
import Network.HPACK2.Buffer
import Network.HPACK2.Huffman.Bit
import Network.HPACK2.Huffman.Params
import Network.HPACK2.Huffman.Table
import Network.HPACK2.Huffman.Tree
import Network.HPACK2.Types (DecodeError(..))

----------------------------------------------------------------

write :: WorkingBuffer -> Word8 -> IO ()
write wbuf w = do
    success <- writeWord8 wbuf w
    unless success $ throwIO TooLongHeaderString

----------------------------------------------------------------

-- | Huffman decoding.
type HuffmanDecoding = Int -> ReadBuffer -> IO ByteString

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
decode :: Buffer -> BufferSize -> HuffmanDecoding
decode buf siz len rbuf = do
    wrkbuf <- newWorkingBuffer buf siz
    dec wrkbuf len rbuf

dec :: WorkingBuffer -> HuffmanDecoding
dec tmp len rbuf = go len (way256 ! 0)
  where
    go 0 way0 = case way0 of
        Way16 Nothing  _ -> throwIO IllegalEos
        Way16 (Just i) _
          | i <= 8       -> toByteString tmp
          | otherwise    -> throwIO TooLongEos
    go !n !way0 = do
        w <- getByte rbuf
        let w0 = w `shiftR` 4
            w1 = w .&. 0x0f
        way1 <- doit way0 w0
        way2 <- doit way1 w1
        go (n - 1) way2
    doit !way !w = case next way w of
        EndOfString -> throwIO EosInTheMiddle
        Forward n   -> return $ way256 ! n
        GoBack  n v -> do
            write tmp v
            return $ way256 ! n

-- | Huffman decoding.
decodeDummy :: HuffmanDecoding
decodeDummy _ _ = return ""

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
