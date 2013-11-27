module BitSpec where

import Network.HPACK.Bit
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "toInt" $ do
        prop "decodes Bits converted by toBits" $ \n ->
            let i = n `mod` 256
            in  toInt (toBits i) == i
