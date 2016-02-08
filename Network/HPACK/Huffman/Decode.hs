{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Network.HPACK2.Huffman.Decode (
  -- * Huffman decoding
    HuffmanDecoding
  , decode
  , decodeDummy
  ) where

import Control.Exception (throwIO)
import Data.Array (Array, (!), listArray)
import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word8)
import Network.HPACK2.Buffer
import Network.HPACK2.Huffman.Bit
import Network.HPACK2.Huffman.Params
import Network.HPACK2.Huffman.Table
import Network.HPACK2.Huffman.Tree
import Network.HPACK2.Types (DecodeError(..))

----------------------------------------------------------------

-- | Huffman decoding.
type HuffmanDecoding = Int -> ReadBuffer -> IO ByteString

----------------------------------------------------------------

data Pin = EndOfString
         | Forward {-# UNPACK #-} !Word8 -- node no.
         | GoBack  {-# UNPACK #-} !Word8 -- node no.
                   {-# UNPACK #-} !Word8 -- a decoded value
         | GoBack2 {-# UNPACK #-} !Word8 -- node no.
                   {-# UNPACK #-} !Word8 -- a decoded value
                   {-# UNPACK #-} !Word8 -- a decoded value
         deriving Show

data WayStep = WayStep !(Maybe Int) !(Array Word8 Pin)
type Way256 = Array Word8 WayStep

next :: WayStep -> Word8 -> Pin
next (WayStep _ a16) w = a16 ! w

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
        WayStep Nothing  _ -> throwIO IllegalEos
        WayStep (Just i) _
          | i <= 8       -> toByteString tmp
          | otherwise    -> throwIO TooLongEos
    go !n !way0 = do
        w <- getByte rbuf
        way <- doit way0 w
        go (n - 1) way
    doit !way !w = case next way w of
        EndOfString -> throwIO EosInTheMiddle
        Forward n   -> return $ way256 ! n
        GoBack  n v -> do
            writeWord8 tmp v
            return $ way256 ! n
        GoBack2 n v1 v2 -> do
            writeWord8 tmp v1
            writeWord8 tmp v2
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
    to16ways x = WayStep ei a16
      where
        !ei = eosInfo x
        !a16 = listArray (0,255) $ map (step decoder x Non) bits8s

data Chara = Non
           | One !Word8
           | Two !Word8 !Word8

inc :: Chara -> Word8 -> Chara
inc Non w     = One w
inc (One v) w = Two v w
inc _       _ = error "inc"

step :: HTree -> HTree -> Chara -> [B] -> Pin
step root (Tip _ v)     x  bss
  | v == idxEos                     = EndOfString
  | otherwise                       = let !w = fromIntegral v
                                          !x' = inc x w
                                      in step root root x' bss
step _    (Bin _ n _ _) Non       [] = Forward (fromIntegral n)
step _    (Bin _ n _ _) (One w)   [] = GoBack (fromIntegral n) w
step _    (Bin _ n _ _) (Two w z) [] = GoBack2 (fromIntegral n) w z
step root (Bin _ _ l _) mx    (F:bs) = step root l mx bs
step root (Bin _ _ _ r) mx    (T:bs) = step root r mx bs

bits8s :: [[B]]
bits8s = [
    [F,F,F,F,F,F,F,F]
  , [F,F,F,F,F,F,F,T]
  , [F,F,F,F,F,F,T,F]
  , [F,F,F,F,F,F,T,T]
  , [F,F,F,F,F,T,F,F]
  , [F,F,F,F,F,T,F,T]
  , [F,F,F,F,F,T,T,F]
  , [F,F,F,F,F,T,T,T]
  , [F,F,F,F,T,F,F,F]
  , [F,F,F,F,T,F,F,T]
  , [F,F,F,F,T,F,T,F]
  , [F,F,F,F,T,F,T,T]
  , [F,F,F,F,T,T,F,F]
  , [F,F,F,F,T,T,F,T]
  , [F,F,F,F,T,T,T,F]
  , [F,F,F,F,T,T,T,T]
  , [F,F,F,T,F,F,F,F]
  , [F,F,F,T,F,F,F,T]
  , [F,F,F,T,F,F,T,F]
  , [F,F,F,T,F,F,T,T]
  , [F,F,F,T,F,T,F,F]
  , [F,F,F,T,F,T,F,T]
  , [F,F,F,T,F,T,T,F]
  , [F,F,F,T,F,T,T,T]
  , [F,F,F,T,T,F,F,F]
  , [F,F,F,T,T,F,F,T]
  , [F,F,F,T,T,F,T,F]
  , [F,F,F,T,T,F,T,T]
  , [F,F,F,T,T,T,F,F]
  , [F,F,F,T,T,T,F,T]
  , [F,F,F,T,T,T,T,F]
  , [F,F,F,T,T,T,T,T]
  , [F,F,T,F,F,F,F,F]
  , [F,F,T,F,F,F,F,T]
  , [F,F,T,F,F,F,T,F]
  , [F,F,T,F,F,F,T,T]
  , [F,F,T,F,F,T,F,F]
  , [F,F,T,F,F,T,F,T]
  , [F,F,T,F,F,T,T,F]
  , [F,F,T,F,F,T,T,T]
  , [F,F,T,F,T,F,F,F]
  , [F,F,T,F,T,F,F,T]
  , [F,F,T,F,T,F,T,F]
  , [F,F,T,F,T,F,T,T]
  , [F,F,T,F,T,T,F,F]
  , [F,F,T,F,T,T,F,T]
  , [F,F,T,F,T,T,T,F]
  , [F,F,T,F,T,T,T,T]
  , [F,F,T,T,F,F,F,F]
  , [F,F,T,T,F,F,F,T]
  , [F,F,T,T,F,F,T,F]
  , [F,F,T,T,F,F,T,T]
  , [F,F,T,T,F,T,F,F]
  , [F,F,T,T,F,T,F,T]
  , [F,F,T,T,F,T,T,F]
  , [F,F,T,T,F,T,T,T]
  , [F,F,T,T,T,F,F,F]
  , [F,F,T,T,T,F,F,T]
  , [F,F,T,T,T,F,T,F]
  , [F,F,T,T,T,F,T,T]
  , [F,F,T,T,T,T,F,F]
  , [F,F,T,T,T,T,F,T]
  , [F,F,T,T,T,T,T,F]
  , [F,F,T,T,T,T,T,T]
  , [F,T,F,F,F,F,F,F]
  , [F,T,F,F,F,F,F,T]
  , [F,T,F,F,F,F,T,F]
  , [F,T,F,F,F,F,T,T]
  , [F,T,F,F,F,T,F,F]
  , [F,T,F,F,F,T,F,T]
  , [F,T,F,F,F,T,T,F]
  , [F,T,F,F,F,T,T,T]
  , [F,T,F,F,T,F,F,F]
  , [F,T,F,F,T,F,F,T]
  , [F,T,F,F,T,F,T,F]
  , [F,T,F,F,T,F,T,T]
  , [F,T,F,F,T,T,F,F]
  , [F,T,F,F,T,T,F,T]
  , [F,T,F,F,T,T,T,F]
  , [F,T,F,F,T,T,T,T]
  , [F,T,F,T,F,F,F,F]
  , [F,T,F,T,F,F,F,T]
  , [F,T,F,T,F,F,T,F]
  , [F,T,F,T,F,F,T,T]
  , [F,T,F,T,F,T,F,F]
  , [F,T,F,T,F,T,F,T]
  , [F,T,F,T,F,T,T,F]
  , [F,T,F,T,F,T,T,T]
  , [F,T,F,T,T,F,F,F]
  , [F,T,F,T,T,F,F,T]
  , [F,T,F,T,T,F,T,F]
  , [F,T,F,T,T,F,T,T]
  , [F,T,F,T,T,T,F,F]
  , [F,T,F,T,T,T,F,T]
  , [F,T,F,T,T,T,T,F]
  , [F,T,F,T,T,T,T,T]
  , [F,T,T,F,F,F,F,F]
  , [F,T,T,F,F,F,F,T]
  , [F,T,T,F,F,F,T,F]
  , [F,T,T,F,F,F,T,T]
  , [F,T,T,F,F,T,F,F]
  , [F,T,T,F,F,T,F,T]
  , [F,T,T,F,F,T,T,F]
  , [F,T,T,F,F,T,T,T]
  , [F,T,T,F,T,F,F,F]
  , [F,T,T,F,T,F,F,T]
  , [F,T,T,F,T,F,T,F]
  , [F,T,T,F,T,F,T,T]
  , [F,T,T,F,T,T,F,F]
  , [F,T,T,F,T,T,F,T]
  , [F,T,T,F,T,T,T,F]
  , [F,T,T,F,T,T,T,T]
  , [F,T,T,T,F,F,F,F]
  , [F,T,T,T,F,F,F,T]
  , [F,T,T,T,F,F,T,F]
  , [F,T,T,T,F,F,T,T]
  , [F,T,T,T,F,T,F,F]
  , [F,T,T,T,F,T,F,T]
  , [F,T,T,T,F,T,T,F]
  , [F,T,T,T,F,T,T,T]
  , [F,T,T,T,T,F,F,F]
  , [F,T,T,T,T,F,F,T]
  , [F,T,T,T,T,F,T,F]
  , [F,T,T,T,T,F,T,T]
  , [F,T,T,T,T,T,F,F]
  , [F,T,T,T,T,T,F,T]
  , [F,T,T,T,T,T,T,F]
  , [F,T,T,T,T,T,T,T]
  , [T,F,F,F,F,F,F,F]
  , [T,F,F,F,F,F,F,T]
  , [T,F,F,F,F,F,T,F]
  , [T,F,F,F,F,F,T,T]
  , [T,F,F,F,F,T,F,F]
  , [T,F,F,F,F,T,F,T]
  , [T,F,F,F,F,T,T,F]
  , [T,F,F,F,F,T,T,T]
  , [T,F,F,F,T,F,F,F]
  , [T,F,F,F,T,F,F,T]
  , [T,F,F,F,T,F,T,F]
  , [T,F,F,F,T,F,T,T]
  , [T,F,F,F,T,T,F,F]
  , [T,F,F,F,T,T,F,T]
  , [T,F,F,F,T,T,T,F]
  , [T,F,F,F,T,T,T,T]
  , [T,F,F,T,F,F,F,F]
  , [T,F,F,T,F,F,F,T]
  , [T,F,F,T,F,F,T,F]
  , [T,F,F,T,F,F,T,T]
  , [T,F,F,T,F,T,F,F]
  , [T,F,F,T,F,T,F,T]
  , [T,F,F,T,F,T,T,F]
  , [T,F,F,T,F,T,T,T]
  , [T,F,F,T,T,F,F,F]
  , [T,F,F,T,T,F,F,T]
  , [T,F,F,T,T,F,T,F]
  , [T,F,F,T,T,F,T,T]
  , [T,F,F,T,T,T,F,F]
  , [T,F,F,T,T,T,F,T]
  , [T,F,F,T,T,T,T,F]
  , [T,F,F,T,T,T,T,T]
  , [T,F,T,F,F,F,F,F]
  , [T,F,T,F,F,F,F,T]
  , [T,F,T,F,F,F,T,F]
  , [T,F,T,F,F,F,T,T]
  , [T,F,T,F,F,T,F,F]
  , [T,F,T,F,F,T,F,T]
  , [T,F,T,F,F,T,T,F]
  , [T,F,T,F,F,T,T,T]
  , [T,F,T,F,T,F,F,F]
  , [T,F,T,F,T,F,F,T]
  , [T,F,T,F,T,F,T,F]
  , [T,F,T,F,T,F,T,T]
  , [T,F,T,F,T,T,F,F]
  , [T,F,T,F,T,T,F,T]
  , [T,F,T,F,T,T,T,F]
  , [T,F,T,F,T,T,T,T]
  , [T,F,T,T,F,F,F,F]
  , [T,F,T,T,F,F,F,T]
  , [T,F,T,T,F,F,T,F]
  , [T,F,T,T,F,F,T,T]
  , [T,F,T,T,F,T,F,F]
  , [T,F,T,T,F,T,F,T]
  , [T,F,T,T,F,T,T,F]
  , [T,F,T,T,F,T,T,T]
  , [T,F,T,T,T,F,F,F]
  , [T,F,T,T,T,F,F,T]
  , [T,F,T,T,T,F,T,F]
  , [T,F,T,T,T,F,T,T]
  , [T,F,T,T,T,T,F,F]
  , [T,F,T,T,T,T,F,T]
  , [T,F,T,T,T,T,T,F]
  , [T,F,T,T,T,T,T,T]
  , [T,T,F,F,F,F,F,F]
  , [T,T,F,F,F,F,F,T]
  , [T,T,F,F,F,F,T,F]
  , [T,T,F,F,F,F,T,T]
  , [T,T,F,F,F,T,F,F]
  , [T,T,F,F,F,T,F,T]
  , [T,T,F,F,F,T,T,F]
  , [T,T,F,F,F,T,T,T]
  , [T,T,F,F,T,F,F,F]
  , [T,T,F,F,T,F,F,T]
  , [T,T,F,F,T,F,T,F]
  , [T,T,F,F,T,F,T,T]
  , [T,T,F,F,T,T,F,F]
  , [T,T,F,F,T,T,F,T]
  , [T,T,F,F,T,T,T,F]
  , [T,T,F,F,T,T,T,T]
  , [T,T,F,T,F,F,F,F]
  , [T,T,F,T,F,F,F,T]
  , [T,T,F,T,F,F,T,F]
  , [T,T,F,T,F,F,T,T]
  , [T,T,F,T,F,T,F,F]
  , [T,T,F,T,F,T,F,T]
  , [T,T,F,T,F,T,T,F]
  , [T,T,F,T,F,T,T,T]
  , [T,T,F,T,T,F,F,F]
  , [T,T,F,T,T,F,F,T]
  , [T,T,F,T,T,F,T,F]
  , [T,T,F,T,T,F,T,T]
  , [T,T,F,T,T,T,F,F]
  , [T,T,F,T,T,T,F,T]
  , [T,T,F,T,T,T,T,F]
  , [T,T,F,T,T,T,T,T]
  , [T,T,T,F,F,F,F,F]
  , [T,T,T,F,F,F,F,T]
  , [T,T,T,F,F,F,T,F]
  , [T,T,T,F,F,F,T,T]
  , [T,T,T,F,F,T,F,F]
  , [T,T,T,F,F,T,F,T]
  , [T,T,T,F,F,T,T,F]
  , [T,T,T,F,F,T,T,T]
  , [T,T,T,F,T,F,F,F]
  , [T,T,T,F,T,F,F,T]
  , [T,T,T,F,T,F,T,F]
  , [T,T,T,F,T,F,T,T]
  , [T,T,T,F,T,T,F,F]
  , [T,T,T,F,T,T,F,T]
  , [T,T,T,F,T,T,T,F]
  , [T,T,T,F,T,T,T,T]
  , [T,T,T,T,F,F,F,F]
  , [T,T,T,T,F,F,F,T]
  , [T,T,T,T,F,F,T,F]
  , [T,T,T,T,F,F,T,T]
  , [T,T,T,T,F,T,F,F]
  , [T,T,T,T,F,T,F,T]
  , [T,T,T,T,F,T,T,F]
  , [T,T,T,T,F,T,T,T]
  , [T,T,T,T,T,F,F,F]
  , [T,T,T,T,T,F,F,T]
  , [T,T,T,T,T,F,T,F]
  , [T,T,T,T,T,F,T,T]
  , [T,T,T,T,T,T,F,F]
  , [T,T,T,T,T,T,F,T]
  , [T,T,T,T,T,T,T,F]
  , [T,T,T,T,T,T,T,T]
  ]

