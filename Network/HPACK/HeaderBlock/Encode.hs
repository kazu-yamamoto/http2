module Network.HPACK.HeaderBlock.Encode (
    toByteStream
  ) where

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as BB
import Data.Bits (setBit)
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
toByteStream :: HuffmanEncoding -> HeaderBlock -> ByteStream
toByteStream he hbs = BB.toByteString $ foldl' (<>) mempty $ map toBB hbs
  where
    toBB = fromHeaderField he

fromHeaderField :: HuffmanEncoding -> HeaderField -> Builder
fromHeaderField _  (Indexed idx)                = index idx
fromHeaderField he (Literal NotAdd (Idx idx) v) = indexedName he set01 idx v
fromHeaderField he (Literal NotAdd (Lit key) v) = newName     he set01 key v
fromHeaderField he (Literal Add    (Idx idx) v) = indexedName he set00 idx v
fromHeaderField he (Literal Add    (Lit key) v) = newName     he set00 key v

----------------------------------------------------------------

index :: Int -> Builder
index = BB.fromWord8 . set1 . I.encodeOne

-- Using Huffman encoding
indexedName :: HuffmanEncoding -> Setter -> Int -> HeaderValue -> Builder
indexedName he set idx v = pre <> vlen <> val
  where
    (p:ps) = I.encode 6 idx
    pre = BB.fromWord8s $ set p : ps
    value = S.encode he v
    valueLen = length value -- FIXME: performance
    vlen = BB.fromWord8s $ setH $ I.encode 7 valueLen
    val = BB.fromWord8s value

-- Using Huffman encoding
newName :: HuffmanEncoding -> Setter -> HeaderName -> HeaderValue -> Builder
newName he set k v = pre <> klen <> key <> vlen <> val
  where
    pre = BB.fromWord8 $ set 0
    key0 = S.encode he k
    keyLen = length key0 -- FIXME: performance
    value = S.encode he v
    valueLen = length value -- FIXME: performance
    klen = BB.fromWord8s $ setH $ I.encode 7 keyLen
    vlen = BB.fromWord8s $ setH $ I.encode 7 valueLen
    key = BB.fromWord8s key0
    val = BB.fromWord8s value

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
