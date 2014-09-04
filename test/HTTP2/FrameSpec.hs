{-# LANGUAGE OverloadedStrings #-}

module HTTP2.FrameSpec where

import Test.Hspec

import qualified Data.ByteString.Char8 as B
import Network.HTTP2.Decode
import Network.HTTP2.Encode
import Network.HTTP2.Types

spec :: Spec
spec = do
    describe "encodeFrameHeader & decodeFrameHeader" $ do
        it "encode/decodes frames properly" $ do
            let header = FrameHeader 500 FrameData 0 (StreamIdentifier 10)
                eheader = decodeFrameHeader defaultSettings $ encodeFrameHeader header
            eheader `shouldBe` Right header

    describe "encodeFrame & decodeFrame" $ do
        it "encode/decodes frames properly" $ do
            let body = "Hello, world!"
                len = B.length body
                header = FrameHeader len FrameData 0 (StreamIdentifier 10)
                frame = Frame header (DataFrame body)
                eframe = decodeFrame defaultSettings $ encodeFrame frame
            eframe `shouldBe` Right frame
