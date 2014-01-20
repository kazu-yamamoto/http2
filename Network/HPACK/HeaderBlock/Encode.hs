module Network.HPACK.HeaderBlock.Encode (
    toByteStream
  ) where

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as BB
import Data.Bits (setBit)
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Monoid ((<>), mempty)
import Data.Word (Word8)
import Network.HPACK.HeaderBlock.HeaderField
import qualified Network.HPACK.HeaderBlock.Integer as I
import qualified Network.HPACK.HeaderBlock.String as S
import Network.HPACK.Huffman
import Network.HPACK.Types

----------------------------------------------------------------

-- | Converting 'HeaderBlock' to the low level format.
toByteStream :: HuffmanEncoding -> Bool -> HeaderBlock -> ByteStream
toByteStream he huff hbs = BB.toByteString $ foldl' (<>) mempty $ map toBB hbs
  where
    toBB = fromHeaderField he huff

fromHeaderField :: HuffmanEncoding -> Bool -> HeaderField -> Builder
fromHeaderField _  _    (Indexed idx)                = index idx
fromHeaderField he huff (Literal NotAdd (Idx idx) v) = indexedName he huff set01 idx v
fromHeaderField he huff (Literal NotAdd (Lit key) v) = newName     he huff set01 key v
fromHeaderField he huff (Literal Add    (Idx idx) v) = indexedName he huff set00 idx v
fromHeaderField he huff (Literal Add    (Lit key) v) = newName     he huff set00 key v

----------------------------------------------------------------

index :: Int -> Builder
index = BB.fromWord8 . set1 . I.encodeOne

-- Using Huffman encoding
indexedName :: HuffmanEncoding -> Bool -> Setter -> Int -> HeaderValue -> Builder
indexedName he huff set idx v = pre <> vlen <> val
  where
    (p:ps) = I.encode 6 idx
    pre = BB.fromWord8s $ set p : ps
    value = S.encode he v
    valueLen = BS.length value
    vlen
      | huff      = BB.fromWord8s $ setH $ I.encode 7 valueLen
      | otherwise = BB.fromWord8s $ I.encode 7 valueLen
    val = BB.fromByteString value

-- Using Huffman encoding
newName :: HuffmanEncoding -> Bool -> Setter -> HeaderName -> HeaderValue -> Builder
newName he huff set k v = pre <> klen <> key <> vlen <> val
  where
    pre = BB.fromWord8 $ set 0
    key0 = S.encode he k
    keyLen = BS.length key0
    value = S.encode he v
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
set1, set01, set00 :: Setter
set1  x = x `setBit` 7
set01 x = x `setBit` 6
set00   = id

setH :: [Word8] -> [Word8]
setH []     = error "setH"
setH (x:xs) = (x `setBit` 7) : xs
