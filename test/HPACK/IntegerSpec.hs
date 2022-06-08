module HPACK.IntegerSpec where

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
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

spec :: Spec
spec = do
    describe "encode and decode" $ do
        prop "duality" $ dual 1
        prop "duality" $ dual 2
        prop "duality" $ dual 3
        prop "duality" $ dual 4
        prop "duality" $ dual 5
        prop "duality" $ dual 6
        prop "duality" $ dual 7
        prop "duality" $ dual 8
