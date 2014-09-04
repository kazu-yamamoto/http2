module HTTP2.FrameSpec where

import Test.Hspec

import Network.HTTP2.Decode
import Network.HTTP2.Encode
import Network.HTTP2.Types

spec :: Spec
spec = do
    describe "encodeFrameHeader" $ do
        it "encodes frames properly" $ do
            let frame = FrameHeader 500 FrameData 0 (StreamIdentifier 10)
                eframe = decodeFrameHeader defaultSettings $ encodeFrameHeader frame
            eframe `shouldBe` Right frame
