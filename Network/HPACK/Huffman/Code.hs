module Network.HPACK.Huffman.Code (
  -- * Huffman encoding
    Encoder
  , toEncoder
  , HuffmanEncoding
  , encode
  -- * Huffman decoding
  , Decoder
  , toDecoder
  , HuffmanDecoding
  , decode
  ) where

import Control.Arrow (second)
import Data.Array (Array, (!), listArray)
import Data.List (partition)
import Data.Word (Word8)
import Network.HPACK.Huffman.Bit
import Network.HPACK.Types (DecodeError(..))

----------------------------------------------------------------

-- | Huffman encoding.
type HuffmanEncoding = [Word8] -> [Word8]

-- | Huffman decoding.
type HuffmanDecoding = [Word8] -> Either DecodeError [Word8]

----------------------------------------------------------------

idxEos :: Int
idxEos = 256

----------------------------------------------------------------

-- | Type for Huffman encoding.
newtype Encoder = Encoder (Array Int Bits)

-- | Creating 'Encoder'.
toEncoder :: [Bits] -> Encoder
toEncoder bs = Encoder $ listArray (0,idxEos) bs

-- | Huffman encoding.
encode :: Encoder -> HuffmanEncoding
encode encoder ws = map fromBits $ group8 bits
  where
    bits = concatMap (enc encoder . fromIntegral) ws
    group8 xs
      | null zs   = eos ys
      | otherwise = ys : group8 zs
      where
        (ys,zs) = splitAt 8 xs
    eos xs
      | len == 0  = []  -- only when ws == [], [] should be encoded to [].
      | len == 8  = [xs]
      | otherwise = [take 8 (xs ++ enc encoder idxEos)]
      where
        len = length xs

enc :: Encoder -> Int -> Bits
enc (Encoder ary) i = ary ! i

----------------------------------------------------------------

-- | Type for Huffman decoding.
data Decoder = Tip (Maybe Int) Int
             | Bin (Maybe Int) Decoder Decoder
             deriving Show

-- | Creating 'Decoder'.
toDecoder :: [Bits] -> Decoder
toDecoder bs = mark 1 eos $ build $ zip [0..idxEos] bs
  where
    eos = bs !! idxEos

build :: [(Int,Bits)] -> Decoder
build [(v,[])] = Tip Nothing v
build xs       = Bin Nothing (build fs) (build ts)
  where
    (fs',ts') = partition ((==) F . head . snd) xs
    fs = map (second tail) fs'
    ts = map (second tail) ts'

mark :: Int -> Bits -> Decoder -> Decoder
mark i []     (Tip Nothing v)   = Tip (Just i) v
mark i (F:bs) (Bin Nothing l r) = Bin (Just i) (mark (i+1) bs l) r
mark i (T:bs) (Bin Nothing l r) = Bin (Just i) l (mark (i+1) bs r)
mark _ _      _                 = error "mark"

-- | Huffman decoding.
decode :: Decoder -> HuffmanDecoding
decode decoder ws = decodeBits decoder (concatMap toBits ws) id

type Builder = [Word8] -> [Word8]

decodeBits :: Decoder -> Bits -> Builder -> Either DecodeError [Word8]
decodeBits decoder xs builder = case dec decoder xs of
  Right (OK v xs') -> decodeBits decoder xs' (builder . (fromIntegral v :))
  Right Eos        -> Right $ builder []
  Left  err        -> Left err

data DecodeOK = Eos | OK Int Bits

dec :: Decoder -> Bits -> Either DecodeError DecodeOK
dec (Tip Nothing v)    xs     = Right $ OK v xs
dec (Tip _       _)    _      = Left EosInTheMiddle
dec (Bin _ l _)        (F:xs) = dec l xs
dec (Bin _ _ r)        (T:xs) = dec r xs
dec (Bin Nothing _ _)  []     = Left IllegalEos
dec (Bin (Just i) _ _) []
  -- i is 1 origin. 8 means Bits are consumed in the parent 7.
  | i <= 8                    = Right Eos
  | otherwise                 = Left TooLongEos
