{-# LANGUAGE CPP #-}

module Network.HPACK2.HeaderBlock.Encode (
    toByteString
  , toBuilder
  ) where

import Data.Bits (setBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Prim as P
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl')
#if __GLASGOW_HASKELL__ < 709
import Data.Monoid (mempty)
#endif
import Data.Monoid ((<>))
import Data.Word (Word8)
import Network.HPACK2.HeaderBlock.HeaderField
import qualified Network.HPACK2.HeaderBlock.Integer as I
import qualified Network.HPACK2.HeaderBlock.String as S

----------------------------------------------------------------

-- | Converting 'HeaderBlock' to the low level format.
toByteString :: Bool -> HeaderBlock -> ByteString
toByteString huff hbs = BL.toStrict $ BB.toLazyByteString $ toBuilder huff hbs

toBuilder :: Bool -> [HeaderField] -> Builder
toBuilder huff hbs = foldl' op mempty hbs
  where
    b `op` x = b <> toBB x
    toBB = fromHeaderField huff

fromHeaderField :: Bool -> HeaderField -> Builder
fromHeaderField _    (ChangeTableSize siz)        = change siz
fromHeaderField _    (Indexed idx)                = index idx
fromHeaderField huff (Literal Add    (Idx idx) v) = indexedName huff 6 set01 idx v
fromHeaderField huff (Literal Add    (Lit key) v) = newName     huff set01 key v
fromHeaderField huff (Literal NotAdd (Idx idx) v) = indexedName huff 4 set0000 idx v
fromHeaderField huff (Literal NotAdd (Lit key) v) = newName     huff set0000 key v
fromHeaderField huff (Literal Never  (Idx idx) v) = indexedName huff 4 set0001 idx v
fromHeaderField huff (Literal Never  (Lit key) v) = newName     huff set0001 key v

----------------------------------------------------------------

word8s :: [Word8] -> Builder
word8s = P.primMapListFixed P.word8

change :: Int -> Builder
change i = word8s (w':ws)
  where
    (w:ws) = I.encode 5 i
    w' = set001 w

index :: Int -> Builder
index i = word8s (w':ws)
  where
    (w:ws) = I.encode 7 i
    w' = set1 w

-- Using Huffman encoding
indexedName :: Bool -> Int -> Setter -> Int -> HeaderValue -> Builder
indexedName huff n set idx v = pre <> vlen <> val
  where
    (p:ps) = I.encode n idx
    pre = word8s $ set p : ps
    value = S.encode huff v
    valueLen = BS.length value
    vlen
      | huff      = word8s $ setH $ I.encode 7 valueLen
      | otherwise = word8s $ I.encode 7 valueLen
    val = BB.byteString value

-- Using Huffman encoding
newName :: Bool -> Setter -> HeaderName -> HeaderValue -> Builder
newName huff set k v = pre <> klen <> key <> vlen <> val
  where
    pre = BB.word8 $ set 0
    key0 = S.encode huff k
    keyLen = BS.length key0
    value = S.encode huff v
    valueLen = BS.length value
    klen
      | huff      = word8s $ setH $ I.encode 7 keyLen
      | otherwise = word8s $ I.encode 7 keyLen
    vlen
      | huff      = word8s $ setH $ I.encode 7 valueLen
      | otherwise = word8s $ I.encode 7 valueLen
    key = BB.byteString key0
    val = BB.byteString value

----------------------------------------------------------------

type Setter = Word8 -> Word8

-- Assuming MSBs are 0.
set1, set01, set001, set0001, set0000 :: Setter
set1    x = x `setBit` 7
set01   x = x `setBit` 6
set001  x = x `setBit` 5
set0001 x = x `setBit` 4
set0000 = id

setH :: [Word8] -> [Word8]
setH []     = error "setH"
setH (x:xs) = (x `setBit` 7) : xs
