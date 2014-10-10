{-# LANGUAGE OverloadedStrings #-}

module HPACK.DecodeSpec where

import Network.HPACK.HeaderBlock
import Network.HPACK.Table
import Network.HPACK.Types
import Test.Hspec

import HPACK.HeaderBlock

spec :: Spec
spec = do
    describe "fromHeaderBlock" $ do
        it "decodes HeaderList in request" $ do
            (c1,h1) <- newHeaderTableForDecoding 4096 >>= flip fromHeaderBlock d41
            h1 `shouldBe` d41h
            (c2,h2) <- fromHeaderBlock c1 d42
            h2 `shouldBe` d42h
            (_,h3)  <- fromHeaderBlock c2 d43
            h3 `shouldBe` d43h
        it "decodes HeaderList in response" $ do
            (c1,h1) <- newHeaderTableForDecoding 256 >>= flip fromHeaderBlock d61
            h1 `shouldBe` d61h
            (c2,h2) <- fromHeaderBlock c1 d62
            h2 `shouldBe` d62h
            (_,h3)  <- fromHeaderBlock c2 d63
            h3 `shouldBe` d63h
        it "decodes HeaderList even if an entry is larger than HeaderTable" $ do
            (c1,h1) <- newHeaderTableForDecoding 64 >>= flip fromHeaderBlock hb1
            h1 `shouldBe` hl1
            isHeaderTableEmpty c1 `shouldBe` True

hb1 :: HeaderBlock
hb1 = [Literal Add (Lit "custom-key") "custom-value"
       -- this is larger than the header table
      ,Literal Add (Lit "loooooooooooooooooooooooooooooooooooooooooog-key")
                        "loooooooooooooooooooooooooooooooooooooooooog-value"
      ]

hl1 :: HeaderList
hl1 = [("custom-key","custom-value")
      ,("loooooooooooooooooooooooooooooooooooooooooog-key"
       ,"loooooooooooooooooooooooooooooooooooooooooog-value")
      ]
