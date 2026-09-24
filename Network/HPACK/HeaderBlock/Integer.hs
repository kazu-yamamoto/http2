module Network.HPACK.HeaderBlock.Integer (
    encodeI,
    encodeInteger,
    decodeI,
    decodeInteger,
    integerLimit,
) where

import qualified Control.Exception as E
import Data.Array (Array, listArray)
import Data.Array.Base (unsafeAt)
import Network.ByteOrder

import Imports
import Network.HPACK.Types (DecodeError (..))

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
    decode m j
        -- Checked before the shift rather than after: shifting an 'Int' by a
        -- word width or more is not defined to give zero, and the value would
        -- have wrapped long before there were anything to notice.
        | m > maxShift = E.throwIO TooLargeInteger
        | otherwise = do
            b <- fromIntegral <$> read8 rbuf
            let d = b .&. 0x7f
            -- d * 2^m > integerLimit - j, without evaluating the product.
            when (d > (integerLimit - j) `shiftR` m) $ E.throwIO TooLargeInteger
            let j' = j + (d `shiftL` m)
            if b `testBit` 7 then decode (m + 7) j' else return j'

-- | The largest integer 'decodeI' will return.
--
-- HPACK's integer encoding carries no bound of its own, so a decoder has to
-- impose one. RFC 7541, section 5.1: "Integer encodings that exceed
-- implementation limits -- in value or octet length -- MUST be treated as
-- decoding errors."
--
-- 2^30 - 1 is far above anything HTTP\/2 can ask for -- a frame payload is at
-- most 2^24 - 1 octets, so no length or index comes near it -- and it still
-- fits in an 'Int' on a platform where that is 32 bits wide.
--
-- >>> integerLimit
-- 1073741823
integerLimit :: Int
integerLimit = 1073741823

-- | The largest shift that can carry a continuation octet into
-- 'integerLimit'; past it every further octet is an overflow.
maxShift :: Int
maxShift = 28
