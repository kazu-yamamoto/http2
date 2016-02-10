{-# LANGUAGE OverloadedStrings #-}

module HPACK.DecodeSpec where

import Network.HPACK
import Network.HPACK.Table
import Test.Hspec

import HPACK.HeaderBlock

spec :: Spec
spec = do
    describe "fromHeaderBlock" $ do
        it "decodes HeaderList in request" $ do
            withDynamicTableForDecoding 4096 4096 $ \dyntabl -> do
                h1 <- decodeHeader dyntabl d41b
                h1 `shouldBe` d41h
                h2 <- decodeHeader dyntabl d42b
                h2 `shouldBe` d42h
                h3 <- decodeHeader dyntabl d43b
                h3 `shouldBe` d43h
        it "decodes HeaderList in response" $ do
            withDynamicTableForDecoding 256 4096 $ \dyntabl -> do
                h1 <- decodeHeader dyntabl d61b
                h1 `shouldBe` d61h
                h2 <- decodeHeader dyntabl d62b
                h2 `shouldBe` d62h
                h3 <- decodeHeader dyntabl d63b
                h3 `shouldBe` d63h
        it "decodes HeaderList even if an entry is larger than DynamicTable" $
            withDynamicTableForEncoding 64 $ \etbl ->
                withDynamicTableForDecoding 64 4096 $ \dtbl -> do
                    hs <- encodeHeader defaultEncodeStrategy 4096 etbl hl1
                    h1 <- decodeHeader dtbl hs
                    h1 `shouldBe` hl1
                    isDynamicTableEmpty etbl `shouldReturn` True
                    isDynamicTableEmpty dtbl `shouldReturn` True

hl1 :: HeaderList
hl1 = [("custom-key","custom-value")
      ,("loooooooooooooooooooooooooooooooooooooooooog-key"
       ,"loooooooooooooooooooooooooooooooooooooooooog-value")
      ]
