module Network.HPACK.HeaderBlock.Format where

import Data.Bits
import qualified Blaze.ByteString.Builder as BB
import qualified Data.ByteString as BS
import Data.CaseInsensitive (foldedCase)
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
    toBB = BB.fromWord8s . fromHeaderField he

fromHeaderField :: HuffmanEncoding -> HeaderField -> [Word8]
fromHeaderField _  (Indexed idx)                = set1  $ I.encode 7 idx
fromHeaderField he (Literal NotAdd (Idx idx) v) = set01 $ indexedName he idx v
fromHeaderField he (Literal NotAdd (Lit key) v) = set01 $ newName he key v
fromHeaderField he (Literal Add    (Idx idx) v) = set00 $ indexedName he idx v
fromHeaderField he (Literal Add    (Lit key) v) = set00 $ newName he key v

indexedName :: HuffmanEncoding -> Int -> HeaderValue -> [Word8]
indexedName he idx v = I.encode 6 idx ++ I.encode 8 vlen ++ S.encode he v
  where
    vlen = BS.length v

newName :: HuffmanEncoding -> HeaderName -> HeaderValue -> [Word8]
newName he key v = 0
                 : I.encode 8 klen ++ S.encode he k
                ++ I.encode 8 vlen ++ S.encode he v
  where
    k = foldedCase key
    klen = BS.length k
    vlen = BS.length v

-- Assuming MSBs are 0.
set1, set01, set00 :: [Word8] -> [Word8]
set1  []     = error "set1"
set1  (x:xs) = setBit x 7 : xs
set01 []     = error "set01"
set01 (x:xs) = setBit x 6 : xs
set00        = id

----------------------------------------------------------------

-- | Converting the low level format to 'HeaderBlock'.
fromByteStream :: HuffmanDecoding -> ByteStream -> HeaderBlock
fromByteStream = undefined

toHeaderField :: HuffmanDecoding -> HeaderField -> HeaderBlock
toHeaderField = undefined
