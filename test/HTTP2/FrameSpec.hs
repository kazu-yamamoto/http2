{-# LANGUAGE OverloadedStrings #-}

module HTTP2.FrameSpec where

import Test.Hspec

import Data.ByteString.Char8 ()
import Network.HTTP2.Decode
import Network.HTTP2.Encode
import Network.HTTP2.Types

spec :: Spec
spec = do
    describe "encodeFrameHeader & decodeFrameHeader" $ do
        it "encode/decodes frames properly" $ do
            let flgs = 0
                fid = 1
                header = FrameHeader 500 flgs (StreamIdentifier 10)
                eheader = decodeFrameHeader defaultSettings $ encodeFrameHeader fid header
            eheader `shouldBe` Right (fid, header)

    describe "encodeFrame & decodeFrame" $ do
        it "encode/decodes frames properly" $ do
            let einfo = EncodeInfo 0 (StreamIdentifier 2) Nothing
                payload = DataFrame "Hello, world!"
                Right frame = decodeFrame defaultSettings $ encodeFrame einfo payload
            framePayload frame `shouldBe` payload
