{-# LANGUAGE OverloadedStrings #-}

module HPACK.DecodeSpec where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.String (fromString)
import Data.Word (Word8)
import Network.HPACK
import Network.HPACK.Table
import Network.HPACK.Token (tokenKey)
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
        it "decodes a Huffman-coded value longer than the Huffman buffer" $
            -- The value decodes to more than the 4096 octets of the
            -- decoder's Huffman buffer.  It used to be reported as a
            -- truncated block, although the same value as a plain literal
            -- was accepted.
            withDynamicTableForEncoding 4096 $ \etbl ->
                withDynamicTableForDecoding 4096 4096 $ \dtbl ->
                    forM_ [False, True] $ \huff -> do
                        let hs = [("x-long", BS8.replicate 5000 'a')]
                            stgy = defaultEncodeStrategy{useHuffman = huff}
                        blk <- encodeHeader stgy 8192 etbl hs
                        decodeHeader dtbl blk `shouldReturn` hs
                        (tvs, _) <- decodeTokenHeader dtbl blk
                        map (\(t, v) -> (tokenKey t, v)) tvs `shouldBe` hs
        it "decodes a block with no fields" $
            -- Empty, or only dynamic table size updates: both are valid
            -- blocks of no fields, and both used to be taken for truncated.
            withDynamicTableForDecoding 4096 4096 $ \dtbl ->
                forM_ ["", "\x20", "\x3f\xe1\x1f"] $ \blk -> do
                    decodeHeader dtbl blk `shouldReturn` []
                    (tvs, _) <- decodeTokenHeader dtbl blk
                    tvs `shouldBe` []
        it "decodes the rest of a block with a malformed field" $
            -- The field after the malformed ones goes into the dynamic
            -- table, and the next block refers to it: index 62, the newest
            -- entry.  The decoder used to stop at the malformed field, so
            -- that reference went astray.
            forM_ [illegalName, tooMany] $ \(fields, err) ->
                withDynamicTableForDecoding 4096 4096 $ \dtbl -> do
                    let blk1 = fields <> incremental "x-after" "2"
                        blk2 = BS.pack [0xbe]
                    decodeTokenHeader dtbl blk1 `shouldThrow` (== err)
                    (tvs, _) <- decodeTokenHeader dtbl blk2
                    map (\(t, v) -> (tokenKey t, v)) tvs `shouldBe` [("x-after", "2")]
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

-- | A field name the encoder would have made lower-case.
illegalName :: (BS.ByteString, DecodeError)
illegalName = (literal "X-Upper" "1", IllegalHeaderName)

-- | One field more than the decoder takes.
tooMany :: (BS.ByteString, DecodeError)
tooMany =
    ( mconcat [literal (BS8.pack ('f' : show i)) "v" | i <- [1 .. 202 :: Int]]
    , TooLargeHeader
    )

-- | A literal field with a new name, without indexing (RFC 7541, 6.2.2).
literal :: BS.ByteString -> BS.ByteString -> BS.ByteString
literal = field 0x00

-- | A literal field with a new name, with incremental indexing (6.2.1).
incremental :: BS.ByteString -> BS.ByteString -> BS.ByteString
incremental = field 0x40

-- | Names and values shorter than 127 octets.
field :: Word8 -> BS.ByteString -> BS.ByteString -> BS.ByteString
field w k v =
    BS.pack [w, fromIntegral (BS.length k)]
        <> k
        <> BS.pack [fromIntegral (BS.length v)]
        <> v

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
