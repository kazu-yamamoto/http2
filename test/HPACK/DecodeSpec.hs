{-# LANGUAGE OverloadedStrings #-}

module HPACK.DecodeSpec where

import Network.HPACK
import Test.Hspec

import HPACK.HeaderBlock

spec :: Spec
spec = do
    describe "fromHeaderBlock" $ do
        it "decodes HeaderList in request" $ do
            dyntabl <- newDynamicTableForDecoding 4096
            h1 <- decodeHeader dyntabl d41b
            h1 `shouldBe` d41h
            h2 <- decodeHeader dyntabl d42b
            h2 `shouldBe` d42h
            h3 <- decodeHeader dyntabl d43b
            h3 `shouldBe` d43h
        it "decodes HeaderList in response" $ do
            dyntabl <- newDynamicTableForDecoding 256
            h1 <- decodeHeader dyntabl d61b
            h1 `shouldBe` d61h
            h2 <- decodeHeader dyntabl d62b
            h2 `shouldBe` d62h
            h3 <- decodeHeader dyntabl d63b
            h3 `shouldBe` d63h
{- fixme
        it "decodes HeaderList even if an entry is larger than DynamicTable" $ do
            dyntabl <- newDynamicTableForDecoding 64
            h1 <- decodeHeader dyntabl undefined
            h1 `shouldBe` hl1
            isDynamicTableEmpty dyntabl `shouldReturn` True
hb1 :: HeaderBlock
hb1 = [Literal Add (Lit "custom-key") "custom-value"
       -- this is larger than the header table
      ,Literal Add (Lit "loooooooooooooooooooooooooooooooooooooooooog-key")
                        "loooooooooooooooooooooooooooooooooooooooooog-value"
      ]
-}

hl1 :: HeaderList
hl1 = [("custom-key","custom-value")
      ,("loooooooooooooooooooooooooooooooooooooooooog-key"
       ,"loooooooooooooooooooooooooooooooooooooooooog-value")
      ]
