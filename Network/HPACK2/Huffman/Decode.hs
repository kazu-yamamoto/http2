module Network.HPACK2.Huffman.Decode (
  -- * Huffman decoding
    HuffmanDecoding
  , decode
  ) where

import Data.ByteString (ByteString)
import Data.Array (Array, (!), listArray)
import Data.Word (Word8)
import Network.HPACK2.Builder.Word8
import Network.HPACK2.Huffman.Bit
import Network.HPACK2.Huffman.ByteString
import Network.HPACK2.Huffman.Params
import Network.HPACK2.Huffman.Table
import Network.HPACK2.Huffman.Tree
import Network.HPACK2.Types (DecodeError(..))

----------------------------------------------------------------

-- | Huffman decoding.
type HuffmanDecoding = ByteString -> Either DecodeError ByteString

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
decode bs = dec qs
  where
    qs = unpack4bits bs

dec :: [Word8] -> Either DecodeError ByteString
dec inp = go (way256 ! 0) inp w8empty
  where
    go :: Way16 -> [Word8] -> Word8Builder -> Either DecodeError ByteString
    go (Way16 Nothing  _) [] _       = Left IllegalEos
    go (Way16 (Just i) _) [] builder
        | i <= 8                     = Right $ toByteString builder
        | otherwise                  = Left TooLongEos
    go way (w:ws) builder = case next way w of
        EndOfString                 -> Left EosInTheMiddle
        Forward n                   -> go (way256 ! n) ws builder
        GoBack  n v                 -> go (way256 ! n) ws (builder <| v)

----------------------------------------------------------------

way256 :: Way256
way256 = construct $ toHTree huffmanTable

construct :: HTree -> Way256
construct decoder = listArray (0,255) $ map to16ways $ flatten decoder
  where
    to16ways x = Way16 ei a16
      where
        ei = eosInfo x
        a16 = listArray (0,15) $ map (step decoder x Nothing) bits4s

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
