{-# LANGUAGE OverloadedStrings #-}

module HPACK.DecodeSpec where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.String (fromString)
import Network.HPACK
import Network.HPACK.Table
import Test.Hspec

import HPACK.HeaderBlock

spec :: Spec
spec = do
    describe "fromHeaderBlock" $ do
        it "decodes [Header] in request" $ do
            withDynamicTableForDecoding 4096 4096 $ \dyntabl -> do
                h1 <- decodeHeader dyntabl d41b
                h1 `shouldBe` d41h
                h2 <- decodeHeader dyntabl d42b
                h2 `shouldBe` d42h
                h3 <- decodeHeader dyntabl d43b
                h3 `shouldBe` d43h
        it "decodes [Header] in response" $ do
            withDynamicTableForDecoding 256 4096 $ \dyntabl -> do
                h1 <- decodeHeader dyntabl d61b
                h1 `shouldBe` d61h
                h2 <- decodeHeader dyntabl d62b
                h2 `shouldBe` d62h
                h3 <- decodeHeader dyntabl d63b
                h3 `shouldBe` d63h
        it "decodes [Header] in response (deny max table size update to 0)" $
            withDynamicTableForDecoding 256 4096 $ \dyntabl -> do
                h1 <- decodeHeader dyntabl d81b
                h1 `shouldBe` d81h
        it "decodes [Header] even if an entry is larger than DynamicTable" $
            withDynamicTableForEncoding 64 $ \etbl ->
                withDynamicTableForDecoding 64 4096 $ \dtbl -> do
                    hs <- encodeHeader defaultEncodeStrategy 4096 etbl hl1
                    h1 <- decodeHeader dtbl hs
                    h1 `shouldBe` hl1
                    isDynamicTableEmpty etbl `shouldReturn` True
                    isDynamicTableEmpty dtbl `shouldReturn` True
        it "keeps the newest entry when a full table evicts" $
            -- A size update to 40 leaves room for one entry.  Two literals
            -- with incremental indexing, then index 62: the newest entry,
            -- "b".  Inserting before evicting used to write "b" over "a",
            -- then evict the slot it had just written, so 62 came back as
            -- a dummy entry.
            withDynamicTableForDecoding 4096 4096 $ \dtbl -> do
                let blk =
                        BS.pack
                            [ 0x3f
                            , 0x09 -- size update: 31 + 9
                            , 0x40
                            , 0x01
                            , 0x61
                            , 0x00 -- a: (incremental)
                            , 0x40
                            , 0x01
                            , 0x62
                            , 0x00 -- b: (incremental)
                            , 0xbe -- indexed 62
                            ]
                decodeHeader dtbl blk `shouldReturn` [("a", ""), ("b", ""), ("b", "")]
        it "round-trips through tables small enough to fill up" $
            -- Entries near the 32-octet minimum fill a table of these sizes
            -- to its last slot.  The encoder follows the peer's
            -- SETTINGS_HEADER_TABLE_SIZE, so any of them can be asked for;
            -- the encoder used to send index 61 of the static table
            -- (www-authenticate) for an entry it had lost.
            forM_ [33, 40, 63, 64, 100, 127, 1023] $ \siz ->
                forM_ [False, True] $ \huff ->
                    withDynamicTableForEncoding siz $ \etbl ->
                        withDynamicTableForDecoding siz 4096 $ \dtbl ->
                            forM_ smallBlocks $ \hs -> do
                                let stgy = defaultEncodeStrategy{useHuffman = huff}
                                blk <- encodeHeader stgy 4096 etbl hs
                                decodeHeader dtbl blk `shouldReturn` hs

-- | Blocks of fields close to the 32-octet minimum entry size, coming back
-- to earlier ones so that the encoder refers to what it inserted.
smallBlocks :: [[Header]]
smallBlocks =
    concat $
        replicate 3 $
            [ [("aa", "x")]
            , [("bb", "y")]
            , [("aa", "x")]
            , [("cc", ""), ("aa", "x")]
            , [("dd", "z"), ("bb", "y"), ("cc", "")]
            ]
                ++ [[(fromString ('k' : show i), "v")] | i <- [0 .. 40 :: Int]]

hl1 :: [Header]
hl1 =
    [ ("custom-key", "custom-value")
    ,
        ( "loooooooooooooooooooooooooooooooooooooooooog-key"
        , "loooooooooooooooooooooooooooooooooooooooooog-value"
        )
    ]
