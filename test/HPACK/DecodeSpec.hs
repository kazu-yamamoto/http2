{-# LANGUAGE OverloadedStrings #-}

module HPACK.DecodeSpec where

import Control.Monad (forM_)
import Data.Bits ((.|.))
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
        it "rejects a field block whose decoded size exceeds the limit" $
            -- CVE-2026-49975: a large value inserted into the dynamic table
            -- once and then referenced expands cheaply on the wire.  The
            -- decoded header list size (RFC 9113 6.5.2) is bounded all the
            -- same.  The crumbs of one Cookie are the worst case, each
            -- reproducing the whole value.
            withDynamicTableForEncoding 4096 $ \etbl ->
                withDynamicTableForDecoding 4096 4096 $ \dtbl -> do
                    setHeaderListSizeLimit (Just (64 * 1024)) dtbl
                    let big = BS8.replicate 4000 'a'
                        many = replicate 20 ("cookie", big) -- ~80 KiB decoded
                    blk <- encodeHeader defaultEncodeStrategy 131072 etbl many
                    decodeTokenHeader dtbl blk `shouldThrow` (== TooLargeHeader)
        it "accepts a field block within the limit" $
            withDynamicTableForEncoding 4096 $ \etbl ->
                withDynamicTableForDecoding 4096 4096 $ \dtbl -> do
                    setHeaderListSizeLimit (Just (64 * 1024)) dtbl
                    let big = BS8.replicate 4000 'a'
                        few = replicate 5 ("cookie", big) -- ~20 KiB decoded
                    blk <- encodeHeader defaultEncodeStrategy 131072 etbl few
                    (tvs, _) <- decodeTokenHeader dtbl blk
                    -- The crumbs are merged into one Cookie value.
                    map (\(t, v) -> (tokenKey t, v)) tvs
                        `shouldBe` [("cookie", BS.intercalate "; " (replicate 5 big))]
        it "does not limit the decoded size when there is no limit" $
            withDynamicTableForEncoding 4096 $ \etbl ->
                withDynamicTableForDecoding 4096 4096 $ \dtbl -> do
                    let big = BS8.replicate 4000 'a'
                        many = replicate 20 ("cookie", big)
                    blk <- encodeHeader defaultEncodeStrategy 131072 etbl many
                    -- No limit by default: decodes fine.
                    (tvs, _) <- decodeTokenHeader dtbl blk
                    length tvs `shouldBe` 1
        it "bounds a field referenced many times in the dynamic table" $
            -- Not only the crumbs of a Cookie: any field the encoder
            -- inserted once and refers to by index afterwards.
            withDynamicTableForEncoding 4096 $ \etbl ->
                withDynamicTableForDecoding 4096 4096 $ \dtbl -> do
                    setHeaderListSizeLimit (Just (64 * 1024)) dtbl
                    let big = BS8.replicate 4000 'a'
                        many = replicate 20 ("x-big", big) -- ~80 KiB decoded
                    blk <- encodeHeader defaultEncodeStrategy 131072 etbl many
                    -- One literal, the rest are references.
                    BS.length blk < 4000 + 100 `shouldBe` True
                    decodeTokenHeader dtbl blk `shouldThrow` (== TooLargeHeader)
        it "bounds the pseudo-header fields too" $
            withDynamicTableForDecoding 4096 4096 $ \dtbl -> do
                setHeaderListSizeLimit (Just 100) dtbl
                -- 5 + 80 + 32 = 117
                let blk = literal ":path" (BS8.replicate 80 'a')
                decodeTokenHeader dtbl blk `shouldThrow` (== TooLargeHeader)
        it "accepts a field block of exactly the limit" $
            -- "x-a: 1" is 3 + 1 + 32 = 36 octets, twice 72.
            withDynamicTableForDecoding 4096 4096 $ \dtbl -> do
                let blk = literal "x-a" "1" <> literal "x-b" "2"
                setHeaderListSizeLimit (Just 72) dtbl
                (tvs, _) <- decodeTokenHeader dtbl blk
                map (\(t, v) -> (tokenKey t, v)) tvs
                    `shouldBe` [("x-a", "1"), ("x-b", "2")]
                setHeaderListSizeLimit (Just 71) dtbl
                decodeTokenHeader dtbl blk `shouldThrow` (== TooLargeHeader)
        it "decodes the rest of a block over the limit" $
            -- As for the other malformed blocks: the field after the one
            -- over the limit still goes into the dynamic table, and the
            -- next block refers to it.
            withDynamicTableForDecoding 4096 4096 $ \dtbl -> do
                setHeaderListSizeLimit (Just 100) dtbl
                let blk1 =
                        literal "x-big" (BS8.replicate 80 'a')
                            <> incremental "x-after" "2"
                    blk2 = BS.pack [0xbe]
                decodeTokenHeader dtbl blk1 `shouldThrow` (== TooLargeHeader)
                (tvs, _) <- decodeTokenHeader dtbl blk2
                map (\(t, v) -> (tokenKey t, v)) tvs `shouldBe` [("x-after", "2")]
        it "applies table size updates as if one after another" $
            -- Applied at once now, two copies of the table at most: the
            -- entries that fit in the smallest size are kept, in a table of
            -- the last size.  "aa: x" and "bb: y" are 35 octets each, and 40
            -- keeps only the newer, index 62.
            withDynamicTableForDecoding 4096 4096 $ \dtbl -> do
                _ <- decodeHeader dtbl $ incremental "aa" "x" <> incremental "bb" "y"
                let ref62 = BS.pack [0xbe]
                    ref63 = BS.pack [0xbf]
                decodeHeader dtbl (sizeUpdate 40 <> sizeUpdate 4096 <> ref62)
                    `shouldReturn` [("bb", "y")]
                decodeHeader dtbl ref63 `shouldThrow` anyException
                -- The table has its last size: room for both again.
                _ <- decodeHeader dtbl $ incremental "cc" "z"
                decodeHeader dtbl (ref62 <> BS.pack [0xbf])
                    `shouldReturn` [("cc", "z"), ("bb", "y")]
        it "takes any number of table size updates" $
            -- RFC 7541 needs two at most; a peer sending thousands, each
            -- one copying the table, made a block costly to decode.
            withDynamicTableForDecoding 4096 4096 $ \dtbl -> do
                _ <- decodeHeader dtbl $ incremental "aa" "x" <> incremental "bb" "y"
                let updates = mconcat $ replicate 1000 $ sizeUpdate 4095 <> sizeUpdate 4096
                decodeHeader dtbl (updates <> BS.pack [0xbe, 0xbf])
                    `shouldReturn` [("bb", "y"), ("aa", "x")]
        it "rejects every field when the limit is zero" $
            -- A limit of zero must not be taken for no limit.
            withDynamicTableForEncoding 4096 $ \etbl ->
                withDynamicTableForDecoding 4096 4096 $ \dtbl -> do
                    setHeaderListSizeLimit (Just 0) dtbl
                    blk <- encodeHeader defaultEncodeStrategy 4096 etbl [("x-a", "1")]
                    decodeTokenHeader dtbl blk `shouldThrow` (== TooLargeHeader)
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

-- | A dynamic table size update (RFC 7541, section 6.3).
sizeUpdate :: Int -> BS.ByteString
sizeUpdate n
    | n < 31 = BS.singleton (0x20 .|. fromIntegral n)
    | otherwise = BS.pack (0x3f : go (n - 31))
  where
    go m
        | m < 128 = [fromIntegral m]
        | otherwise = fromIntegral (m `mod` 128 + 128) : go (m `div` 128)

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
