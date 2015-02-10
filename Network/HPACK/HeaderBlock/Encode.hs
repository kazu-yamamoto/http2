module Network.HPACK.HeaderBlock.Encode (
    toByteString
  ) where

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as BB
import Data.Bits (setBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Monoid ((<>), mempty)
import Data.Word (Word8)
import Network.HPACK.HeaderBlock.HeaderField
import qualified Network.HPACK.HeaderBlock.Integer as I
import qualified Network.HPACK.HeaderBlock.String as S

----------------------------------------------------------------

-- | Converting 'HeaderBlock' to the low level format.
toByteString :: Bool -> HeaderBlock -> ByteString
toByteString huff hbs = BB.toByteString $ foldl' (<>) mempty $ map toBB hbs
  where
    toBB = fromHeaderField huff

fromHeaderField :: Bool -> HeaderField -> Builder
fromHeaderField _    (ChangeTableSize siz)        = change siz
fromHeaderField _    (Indexed idx)                = index idx
fromHeaderField huff (Literal Add    (Idx idx) v) = indexedName huff 6 set01 idx v
fromHeaderField huff (Literal Add    (Lit key) v) = newName     huff set01 key v
fromHeaderField huff (Literal NotAdd (Idx idx) v) = indexedName huff 4 set0000 idx v
fromHeaderField huff (Literal NotAdd (Lit key) v) = newName     huff set0000 key v
fromHeaderField huff (Literal Never (Idx idx) v)  = indexedName huff 4 set0001 idx v
fromHeaderField huff (Literal Never (Lit key) v)  = newName     huff set0001 key v

----------------------------------------------------------------

change :: Int -> Builder
change i = BB.fromWord8s (w':ws)
  where
    (w:ws) = I.encode 5 i
    w' = set001 w

index :: Int -> Builder
index i = BB.fromWord8s (w':ws)
  where
    (w:ws) = I.encode 7 i
    w' = set1 w

-- Using Huffman encoding
indexedName :: Bool -> Int -> Setter -> Int -> HeaderValue -> Builder
indexedName huff n set idx v = pre <> vlen <> val
  where
    (p:ps) = I.encode n idx
    pre = BB.fromWord8s $ set p : ps
    value = S.encode huff v
    valueLen = BS.length value
    vlen
      | huff      = BB.fromWord8s $ setH $ I.encode 7 valueLen
      | otherwise = BB.fromWord8s $ I.encode 7 valueLen
    val = BB.fromByteString value

-- Using Huffman encoding
newName :: Bool -> Setter -> HeaderName -> HeaderValue -> Builder
newName huff set k v = pre <> klen <> key <> vlen <> val
  where
    pre = BB.fromWord8 $ set 0
    key0 = S.encode huff k
    keyLen = BS.length key0
    value = S.encode huff v
    valueLen = BS.length value
    klen
      | huff      = BB.fromWord8s $ setH $ I.encode 7 keyLen
      | otherwise = BB.fromWord8s $ I.encode 7 keyLen
    vlen
      | huff      = BB.fromWord8s $ setH $ I.encode 7 valueLen
      | otherwise = BB.fromWord8s $ I.encode 7 valueLen
    key = BB.fromByteString key0
    val = BB.fromByteString value

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
