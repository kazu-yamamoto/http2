module HPACK.IntegerSpec where

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Network.HPACK (DecodeError (..))
import Network.HPACK.Internal
import Test.Hspec
import Test.Hspec.QuickCheck

dual :: Int -> Int -> Expectation
dual n i = do
    let x = abs i
    bs <- encodeInteger n x
    let (w, ws) = fromMaybe (error "dual") $ BS.uncons bs
    x' <- decodeInteger n w ws
    x `shouldBe` x'

roundtrip7 :: BS.ByteString -> IO Int
roundtrip7 bs = do
    let (w, ws) = fromMaybe (error "roundtrip7") $ BS.uncons bs
    decodeInteger 7 w ws

-- | Decode with a 7-bit prefix that is all ones, so that the continuation
-- octets in 'ws' are what decides the value.
decode7 :: [Word8] -> IO Int
decode7 ws = decodeInteger 7 127 (BS.pack ws)

spec :: Spec
spec = do
    describe "decodeInteger" $ do
        it "rejects an encoding that runs past the limit" $ do
            r <- encodeInteger 7 integerLimit >>= roundtrip7
            r `shouldBe` integerLimit
            ws <- BS.unpack . BS.tail <$> encodeInteger 7 (integerLimit + 1)
            decode7 ws `shouldThrow` (== TooLargeInteger)

        it "rejects an encoding in more octets than the limit can take" $
            -- Continuation octets that each add nothing, so only their number
            -- is objectionable.
            decode7 (replicate 8 0x80 ++ [0x00]) `shouldThrow` (== TooLargeInteger)

        it "rejects an encoding that would wrap around" $
            -- This used to come back as 2, by overflowing 'Int' until it
            -- landed there: the same as the single octet 0x82, ":method: GET".
            -- Two byte strings decoding alike is exactly what RFC 7541
            -- section 5.1 asks a decoder to refuse.
            decode7 [0x83, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01]
                `shouldThrow` (== TooLargeInteger)

    describe "encode and decode" $ do
        prop "duality" $ dual 1
        prop "duality" $ dual 2
        prop "duality" $ dual 3
        prop "duality" $ dual 4
        prop "duality" $ dual 5
        prop "duality" $ dual 6
        prop "duality" $ dual 7
        prop "duality" $ dual 8
