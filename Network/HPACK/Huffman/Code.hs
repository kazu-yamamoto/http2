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
  , printTree
  , step -- fixme
  , bits4s
  , flatten
  , doit
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Data.Array (Array, (!), listArray)
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString as BS
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
data HTree = Tip
             (Maybe Int)          -- EOS info from 1
             {-# UNPACK #-} !Int  -- Decoded value. Essentially Word8
           | Bin (Maybe Int)      -- EOS info from 1
             {-# UNPACK #-} !Int  -- Sequence no from 0
             HTree              -- Left
             HTree              -- Right
           deriving Show

showTree :: HTree -> String
showTree = showTree' ""

showTree' :: String -> HTree -> String
showTree' _    (Tip _ i)     = show i ++ "\n"
showTree' pref (Bin _ n l r) = "No " ++ show n ++ "\n"
                            ++ pref ++ "+ " ++ showTree' pref' l
                            ++ pref ++ "+ " ++ showTree' pref' r
  where
    pref' = "  " ++ pref

printTree :: HTree -> IO ()
printTree = putStr . showTree

-- | Creating 'HTree'.
toHTree :: [Bits] -> HTree
toHTree bs = mark 1 eos $ snd $ build 0 $ zip [0..idxEos] bs
  where
    eos = bs !! idxEos

build :: Int -> [(Int,Bits)] -> (Int, HTree)
build !cnt0 [(v,[])] = (cnt0,Tip Nothing v)
build !cnt0 xs       = let (cnt1,l) = build (cnt0 + 1) fs
                           (cnt2,r) = build cnt1 ts
                       in (cnt2, Bin Nothing cnt0 l r)
  where
    (fs',ts') = partition ((==) F . head . snd) xs
    fs = map (second tail) fs'
    ts = map (second tail) ts'

mark :: Int -> Bits -> HTree -> HTree
mark i []     (Tip Nothing v)     = Tip (Just i) v
mark i (F:bs) (Bin Nothing n l r) = Bin (Just i) n (mark (i+1) bs l) r
mark i (T:bs) (Bin Nothing n l r) = Bin (Just i) n l (mark (i+1) bs r)
mark _ _      _                   = error "mark"

type Way16  = Array Word8 Pin
type Way256 = Array Word8 Way16

newtype Decoder = Decoder Way256

toDecoder :: [Bits] -> Decoder
toDecoder = doit . toHTree

-- | Huffman decoding.
decode :: Decoder -> HuffmanDecoding
decode (Decoder aoa) bs = dec aoa qs
  where
    qs = toQ $ BS.unpack bs -- fixme
    toQ [] = []
    toQ (w:ws) = w0 : w1 : toQ ws
      where
        w0 = w `shiftR` 4
        w1 = w .&. 0xf

dec :: Way256 -> [Word8] -> Either DecodeError ByteString
dec way256 inp = loop (way256 ! 0) inp w8empty
  where
    loop :: Way16 -> [Word8] -> Word8Builder -> Either DecodeError ByteString
    loop _   []     builder = Right $ toByteString builder
    loop way (w:ws) builder = case way ! w of
        EndOfString -> undefined
        Forward n   -> loop (way256 ! n) ws builder
        GoBack  n v -> loop (way256 ! n) ws (builder <| v)

flatten :: HTree -> [HTree]
flatten (Tip _ _)       = []
flatten t@(Bin _ _ l r) = t : (flatten l ++ flatten r)

doit :: HTree -> Decoder
doit decoder = Decoder $ listArray (0,255) $ map to16ways $ flatten decoder
  where
    to16ways x = listArray (0,15) $ map (step decoder x Nothing) bits4s

data Pin = EndOfString
         | Forward Word8
         | GoBack  Word8 Word8
         deriving Show

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
