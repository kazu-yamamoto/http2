module Network.HPACK.Builder.Word8 where

import Data.ByteString.Internal (ByteString, unsafeCreate)
import Data.Word (Word8)
import Foreign.Storable (poke)
import Foreign.Ptr (plusPtr)

data Word8Builder = Word8Builder !Int ([Word8] -> [Word8])

(<|) :: Word8Builder -> Word8 -> Word8Builder
Word8Builder i b <| w = Word8Builder (i+1) $ b . (w :)

w8empty :: Word8Builder
w8empty = Word8Builder 0 id

{-
singleton :: Word8 -> Word8Builder
singleton x = Word8Builder 1 (x :)
-}

toByteString :: Word8Builder -> ByteString
toByteString (Word8Builder i b) = unsafeCreate i $ \ptr -> go ptr ws
  where
    ws = b []
    go _   []     = return ()
    go ptr (x:xs) = do
        poke ptr x
        go (ptr `plusPtr` 1) xs
