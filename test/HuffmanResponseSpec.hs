module HuffmanResponseSpec where

import Network.HPACK.HuffmanResponse
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "decodeInResponse" $ do
        prop "decodes sequence encoded by encodeInResponse" $ \ns ->
            let is = map ((`mod` 255) . abs) ns
            in decodeInResponse (encodeInResponse is) == is
