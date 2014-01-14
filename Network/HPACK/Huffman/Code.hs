{-# LANGUAGE BangPatterns #-}

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

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Data.Array (Array, (!), listArray)
import Data.Bits ((.&.), shiftR)
import Data.ByteString.Internal (ByteString(..))
import Data.List (partition)
import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek)
import Network.HPACK.Builder
import Network.HPACK.Builder.Word8
import Network.HPACK.Huffman.Bit
import Network.HPACK.Types (DecodeError(..))
import System.IO.Unsafe (unsafePerformIO)

----------------------------------------------------------------

-- | Huffman encoding.
type HuffmanEncoding = ByteString -> ByteString

-- | Huffman decoding.
type HuffmanDecoding = ByteString -> Either DecodeError ByteString

----------------------------------------------------------------

idxEos :: Int
idxEos = 256

----------------------------------------------------------------

-- | Type for Huffman encoding.
newtype Encoder = Encoder (Array Int (Int,Bits))

-- | Creating 'Encoder'.
toEncoder :: [Bits] -> Encoder
toEncoder bss = Encoder $ listArray (0,idxEos) (map toEnt bss)
  where
    toEnt bs = (len, bs)
      where
        !len = length bs

-- | Huffman encoding.
encode :: Encoder -> HuffmanEncoding
encode encoder (PS fptr off len) = fromBitsToByteString nBytes (run b)
  where
    (nbits,b0) = unsafePerformIO $ withForeignPtr fptr $ \ptr ->
        loop (ptr `plusPtr` off) 0 0 empty
    (nBytes,b) = eos nbits b0
    loop :: Ptr Word8 -> Int -> Int -> Builder Bits -> IO (Int,Builder Bits)
    loop !ptr !cnt !nBits !builder
      | cnt == len = return (nBits,builder)
      | otherwise  = do
          i <- fromIntegral <$> peek ptr
          let (bits,bs) = enc encoder i
              builder' = builder << bs
          loop (ptr `plusPtr` 1) (cnt + 1) (nBits + bits) builder'
    eos nBits !builder
      | r == 0    = (q,builder)
      | otherwise = (q+1, builder << bools')
      where
--        (q,r) = nBits `divMod` 8
        q = nBits `shiftR` 3
        r = nBits .&. 0x7
        (_,bools) = enc encoder idxEos
        bools' = take (8 - r) bools

enc :: Encoder -> Int -> (Int,Bits)
enc (Encoder ary) i = ary ! i

----------------------------------------------------------------

-- | Type for Huffman decoding.
data Decoder = Tip (Maybe Int) {-# UNPACK #-} !Int
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
decode decoder bs = decodeBits decoder src w8empty
  where
    src = toBitSource bs

decodeBits :: Decoder -> BitSource -> Word8Builder -> Either DecodeError ByteString
decodeBits decoder src builder = case dec decoder src of
  Right (OK v src') -> decodeBits decoder src' (builder <| fromIntegral v)
  Right Eos         -> Right $ toByteString builder
  Left  err         -> Left err

data DecodeOK = Eos | OK Int BitSource

dec :: Decoder -> BitSource -> Either DecodeError DecodeOK
dec (Tip Nothing v)    src = Right $ OK v src
dec (Tip _       _)    _   = Left EosInTheMiddle
dec (Bin x l r)        src = case uncons src of
    Nothing -> case x of
        Nothing           -> Left IllegalEos
        Just i
          -- i is 1 origin. 8 means Bits are consumed in the parent 7.
          | i <= 8        -> Right Eos
          | otherwise     -> Left TooLongEos
    Just (F,src')         -> dec l src'
    Just (T,src')         -> dec r src'
