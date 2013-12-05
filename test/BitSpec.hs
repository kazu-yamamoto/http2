module BitSpec where

import Network.HPACK.Huffman.Bit
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "toBits and toInt" $ do
        prop "duality" $ \n ->
            let i = n `mod` 256
            in  toInt (toBits i) == i
