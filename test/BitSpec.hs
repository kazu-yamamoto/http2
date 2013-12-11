module BitSpec where

import Network.HPACK.Huffman.Bit
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "toBits and fromBits" $ do
        prop "duality" $ \n ->
            let i = fromIntegral $ abs (n :: Int) `mod` 256
            in  fromBits (toBits i) == i
