module Network.HPACK.HeaderBlock.Integer (
    encodeI,
    encodeInteger,
    decodeI,
    decodeInteger,
) where

import Data.Array (Array, listArray)
import Data.Array.Base (unsafeAt)
import Network.ByteOrder

import Imports

-- $setup
-- >>> import qualified Data.ByteString as BS

powerArray :: Array Int Int
powerArray = listArray (1, 8) [1, 3, 7, 15, 31, 63, 127, 255]

----------------------------------------------------------------

{-
if I < 2^N - 1, encode I on N bits
   else
       encode (2^N - 1) on N bits
       I = I - (2^N - 1)
       while I >= 128
            encode (I % 128 + 128) on 8 bits
            I = I / 128
       encode I on 8 bits
-}

-- | Encoding integer with a temporary buffer whose size is 4096.
--   No prefix is set.
--
-- >>> BS.unpack <$> encodeInteger 5 10
-- [10]
-- >>> BS.unpack <$> encodeInteger 5 1337
-- [31,154,10]
-- >>> BS.unpack <$> encodeInteger 8 42
-- [42]
encodeInteger
    :: Int
    -- ^ N+
    -> Int
    -- ^ Target
    -> IO ByteString
encodeInteger n i = withWriteBuffer 4096 $ \wbuf -> encodeI wbuf id n i

-- Using write8 is faster than using internals directly.
--

-- | Integer encoding with a write buffer.
{-# INLINEABLE encodeI #-}
encodeI
    :: WriteBuffer
    -> (Word8 -> Word8)
    -- ^ Setting prefix
    -> Int
    -- ^ N+
    -> Int
    -- ^ Target
    -> IO ()
encodeI wbuf set n i
    | i < p = write8 wbuf $ set $ fromIntegral i
    | otherwise = do
        write8 wbuf $ set $ fromIntegral p
        encode' (i - p)
  where
    p = powerArray `unsafeAt` (n - 1)
    encode' :: Int -> IO ()
    encode' j
        | j < 128 = write8 wbuf $ fromIntegral j
        | otherwise = do
            let q = j `shiftR` 7
                r = j .&. 0x7f
            write8 wbuf $ fromIntegral (r + 128)
            encode' q

----------------------------------------------------------------

{-
decode I from the next N bits
   if I < 2^N - 1, return I
   else
       M = 0
       repeat
           B = next octet
           I = I + (B & 127) * 2^M
           M = M + 7
       while B & 128 == 128
       return I
-}

-- | Integer decoding. The first argument is N of prefix.
--
-- >>> decodeInteger 5 10 $ BS.empty
-- 10
-- >>> decodeInteger 5 31 $ BS.pack [154,10]
-- 1337
-- >>> decodeInteger 8 42 $ BS.empty
-- 42
decodeInteger
    :: Int
    -- ^ N+
    -> Word8
    -- ^ The head of encoded integer whose prefix is already dropped
    -> ByteString
    -- ^ The tail of encoded integer
    -> IO Int
decodeInteger n w bs = withReadBuffer bs $ \rbuf -> decodeI n w rbuf

{-# INLINEABLE decodeI #-}

-- | Integer decoding with a read buffer. The first argument is N of prefix.
decodeI
    :: Int
    -- ^ N+
    -> Word8
    -- ^ The head of encoded integer whose prefix is already dropped
    -> ReadBuffer
    -> IO Int
decodeI n w rbuf
    | i < p = return i
    | otherwise = decode 0 i
  where
    p = powerArray `unsafeAt` (n - 1)
    i = fromIntegral w
    decode :: Int -> Int -> IO Int
    decode m j = do
        b <- fromIntegral <$> read8 rbuf
        let j' = j + (b .&. 0x7f) * 2 ^ m
            m' = m + 7
            cont = b `testBit` 7
        if cont then decode m' j' else return j'
