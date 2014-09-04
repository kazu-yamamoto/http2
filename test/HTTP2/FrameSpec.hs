module HTTP2.FrameSpec where

import Data.Attoparsec.ByteString
import Blaze.ByteString.Builder
import Data.ByteString.Lazy
import Test.Hspec

import Network.HTTP2.Decode
import Network.HTTP2.Encode
import Network.HTTP2.Types

spec :: Spec
spec = do
    describe "encodeFrameHeader" $ do
        it "encodes frames properly" $ do
            let frame = FrameHeader 500 0 0 (StreamIdentifier 10)
                eframe = parseOnly (decodeFrameHeader defaultSettings) $ toStrict $ toLazyByteString $ encodeFrameHeader frame
            eframe `shouldBe` Right frame
