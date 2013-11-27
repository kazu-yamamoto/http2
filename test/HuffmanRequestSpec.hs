module HuffmanRequestSpec where

import Network.HPACK.HuffmanRequest
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "decodeInRequest" $ do
        prop "decodes sequence encoded by encodeInRequest" $ \ns ->
            let is = map ((`mod` 255) . abs) ns
            in decodeInRequest (encodeInRequest is) == is
