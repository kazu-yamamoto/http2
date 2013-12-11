module IntegerSpec where

import Network.HPACK.HeaderBlock.Integer
import Test.Hspec
import Test.Hspec.QuickCheck

dual :: Int -> Int -> Bool
dual n i = decode n (encode n x) == x
  where
    x = abs i

spec :: Spec
spec = do
    describe "encode and dcode" $ do
        prop "duality" $ dual 1
        prop "duality" $ dual 2
        prop "duality" $ dual 3
        prop "duality" $ dual 4
        prop "duality" $ dual 5
        prop "duality" $ dual 6
        prop "duality" $ dual 7
        prop "duality" $ dual 8
