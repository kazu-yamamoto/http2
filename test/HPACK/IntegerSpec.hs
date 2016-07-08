module HPACK.IntegerSpec where

import Network.HPACK.HeaderBlock.Integer
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.ByteString as BS

dual :: Int -> Int -> Expectation
dual n i = do
    let x = abs i
    bs <- encodeInteger n x
    let Just (w, ws) = BS.uncons bs
    x' <- decodeInteger n w ws
    x `shouldBe` x'

cal :: Int -> Int -> Expectation
cal n i = do
    let x = abs i
        len = calLen n x
    len' <- BS.length <$> encodeInteger n x
    len `shouldBe` len'

spec :: Spec
spec = do
    describe "calLne" $ do
        prop "calLen" $ cal 1
        prop "calLen" $ cal 2
        prop "calLen" $ cal 3
        prop "calLen" $ cal 4
        prop "calLen" $ cal 5
        prop "calLen" $ cal 6
        prop "calLen" $ cal 7
        prop "calLen" $ cal 8
    describe "encode and decode" $ do
        prop "duality" $ dual 1
        prop "duality" $ dual 2
        prop "duality" $ dual 3
        prop "duality" $ dual 4
        prop "duality" $ dual 5
        prop "duality" $ dual 6
        prop "duality" $ dual 7
        prop "duality" $ dual 8
