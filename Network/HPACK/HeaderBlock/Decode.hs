{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HPACK.HeaderBlock.Decode (
    decodeHeader,
    decodeTokenHeader,
    ValueTable,
    TokenHeaderTable,
    toTokenHeaderTable,
    getFieldValue,
    decodeString,
    decodeS,
    decodeSophisticated,
    decodeSimple, -- testing
) where

import qualified Control.Exception as E
import Data.Array.Base (unsafeRead, unsafeWrite)
import qualified Data.Array.IO as IOA
import qualified Data.Array.Unsafe as Unsafe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Char (isUpper)
import Network.ByteOrder
import Network.HTTP.Semantics

import Imports hiding (empty)
import Network.HPACK.Builder
import Network.HPACK.HeaderBlock.Integer
import Network.HPACK.Huffman
import Network.HPACK.Table
import Network.HPACK.Types

----------------------------------------------------------------

-- | Converting the HPACK format to '[Header]'.
--
--   * Headers are decoded as is.
--   * 'DecodeError' would be thrown if the HPACK format is broken.
decodeHeader
    :: DynamicTable
    -> ByteString
    -- ^ An HPACK format
    -> IO [Header]
decodeHeader dyntbl inp = decodeHPACK dyntbl inp (decodeSimple (toTokenHeader dyntbl))

-- | Converting the HPACK format to 'TokenHeaderList'
--   and 'ValueTable'.
--
--   * Multiple values of Cookie: are concatenated.
--   * If a pseudo header appears multiple times,
--     'IllegalHeaderName' is thrown.
--   * If unknown pseudo headers appear,
--     'IllegalHeaderName' is thrown.
--   * If pseudo headers are found after normal headers,
--     'IllegalHeaderName' is thrown.
--   * If a header key contains capital letters,
--     'IllegalHeaderName' is thrown.
--   * If the number of header fields is too large,
--     'TooLargeHeader' is thrown.
--   * 'IllegalHeaderName' and 'TooLargeHeader' are thrown only once the
--     whole block has been decoded, so that the dynamic table is up to
--     date: the message is malformed, not the block.
--   * 'DecodeError' would be thrown if the HPACK format is broken.
decodeTokenHeader
    :: DynamicTable
    -> ByteString
    -- ^ An HPACK format
    -> IO TokenHeaderTable
decodeTokenHeader dyntbl inp =
    decodeHPACK dyntbl inp (decodeSophisticated (toTokenHeader dyntbl)) `E.catch` \BufferOverrun -> E.throwIO HeaderBlockTruncated

decodeHPACK
    :: DynamicTable
    -> ByteString
    -> (ReadBuffer -> IO a)
    -> IO a
decodeHPACK dyntbl inp dec = withReadBuffer inp chkChange
  where
    chkChange rbuf = do
        -- A block can be empty, or hold nothing but table size updates:
        -- no fields, which is what empty trailers are sent as.  Reading
        -- on regardless threw 'BufferOverrun', reported as a truncated
        -- block, so the connection was closed over them.
        leftover <- remainingSize rbuf
        if leftover < 1
            then dec rbuf
            else do
                w <- read8 rbuf
                if isTableSizeUpdate w
                    then do
                        tableSizeUpdate dyntbl w rbuf
                        chkChange rbuf
                    else do
                        ff rbuf (-1)
                        dec rbuf

-- | Converting to '[Header]'.
--
--   * Headers are decoded as is.
--   * 'DecodeError' would be thrown if the HPACK format is broken.
decodeSimple
    :: (Word8 -> ReadBuffer -> IO TokenHeader)
    -> ReadBuffer
    -> IO [Header]
decodeSimple decTokenHeader rbuf = go empty
  where
    go builder = do
        leftover <- remainingSize rbuf
        if leftover >= 1
            then do
                w <- read8 rbuf
                tv <- decTokenHeader w rbuf
                let builder' = builder << tv
                go builder'
            else do
                let tvs = run builder
                    kvs = map (\(t, v) -> let k = tokenKey t in (k, v)) tvs
                return kvs

headerLimit :: Int
headerLimit = 200

-- | Converting to 'TokenHeaderList' and 'ValueTable'.
--
--   * Multiple values of Cookie: are concatenated.
--   * If a pseudo header appears multiple times,
--     'IllegalHeaderName' is thrown.
--   * If unknown pseudo headers appear,
--     'IllegalHeaderName' is thrown.
--   * If pseudo headers are found after normal headers,
--     'IllegalHeaderName' is thrown.
--   * If a header key contains capital letters,
--     'IllegalHeaderName' is thrown.
--   * If the number of header fields is too large,
--     'TooLargeHeader' is thrown
--   * 'IllegalHeaderName' and 'TooLargeHeader' are thrown only once the
--     whole block has been decoded, so that the dynamic table is up to
--     date: the message is malformed, not the block.
--   * 'DecodeError' would be thrown if the HPACK format is broken.
decodeSophisticated
    :: (Word8 -> ReadBuffer -> IO TokenHeader)
    -> ReadBuffer
    -> IO TokenHeaderTable
decodeSophisticated decTokenHeader rbuf = do
    -- using maxTokenIx to reduce condition
    arr <- IOA.newArray (minTokenIx, maxTokenIx) Nothing
    tvs <- pseudoNormal arr
    tbl <- Unsafe.unsafeFreeze arr
    return (tvs, tbl)
  where
    pseudoNormal :: IOA.IOArray Int (Maybe FieldValue) -> IO TokenHeaderList
    pseudoNormal arr = pseudo
      where
        pseudo = do
            leftover <- remainingSize rbuf
            if leftover >= 1
                then do
                    w <- read8 rbuf
                    tv@(Token{..}, v) <- decTokenHeader w rbuf
                    if isPseudo
                        then do
                            mx <- unsafeRead arr tokenIx
                            -- duplicated
                            when (isJust mx) $ malformed IllegalHeaderName
                            -- unknown
                            when (isMaxTokenIx tokenIx) $ malformed IllegalHeaderName
                            unsafeWrite arr tokenIx (Just v)
                            pseudo
                        else do
                            -- 0-Length Headers Leak - CVE-2019-9516
                            when (tokenKey == "") $ malformed IllegalHeaderName
                            when (isMaxTokenIx tokenIx && B8.any isUpper (original tokenKey)) $
                                malformed IllegalHeaderName
                            unsafeWrite arr tokenIx (Just v)
                            if isCookieTokenIx tokenIx
                                then normal 0 empty (empty << v)
                                else normal 0 (empty << tv) empty
                else return []
        normal n builder cookie
            | n > headerLimit = malformed TooLargeHeader
            | otherwise = do
                leftover <- remainingSize rbuf
                if leftover >= 1
                    then do
                        w <- read8 rbuf
                        tv@(Token{..}, v) <- decTokenHeader w rbuf
                        when isPseudo $ malformed IllegalHeaderName
                        -- 0-Length Headers Leak - CVE-2019-9516
                        when (tokenKey == "") $ malformed IllegalHeaderName
                        when (isMaxTokenIx tokenIx && B8.any isUpper (original tokenKey)) $
                            malformed IllegalHeaderName
                        unsafeWrite arr tokenIx (Just v)
                        if isCookieTokenIx tokenIx
                            then normal (n + 1) builder (cookie << v)
                            else normal (n + 1) (builder << tv) cookie
                    else do
                        let tvs0 = run builder
                            cook = run cookie
                        if null cook
                            then return tvs0
                            else do
                                let v = BS.intercalate "; " cook
                                    tvs = (tokenCookie, v) : tvs0
                                unsafeWrite arr cookieTokenIx (Just v)
                                return tvs

    -- A field that makes the message malformed, as opposed to the block.
    -- The rest of the block is decoded all the same, and only then is the
    -- error thrown: every field of it may change the dynamic table, and one
    -- left undecoded leaves our table out of step with the peer's encoder,
    -- so that nothing after it on the connection decodes.  So decoded, a
    -- malformed message can be refused on its own (RFC 9113, section 8.1.1:
    -- a stream error), rather than with the connection.
    malformed :: DecodeError -> IO a
    malformed err = skipRest >> E.throwIO err
    skipRest = do
        leftover <- remainingSize rbuf
        when (leftover >= 1) $ do
            w <- read8 rbuf
            _ <- decTokenHeader w rbuf
            skipRest

toTokenHeader :: DynamicTable -> Word8 -> ReadBuffer -> IO TokenHeader
toTokenHeader dyntbl w rbuf
    | w `testBit` 7 = indexed dyntbl w rbuf
    | w `testBit` 6 = incrementalIndexing dyntbl w rbuf
    | w `testBit` 5 = E.throwIO IllegalTableSizeUpdate
    | w `testBit` 4 = neverIndexing dyntbl w rbuf
    | otherwise = withoutIndexing dyntbl w rbuf

tableSizeUpdate :: DynamicTable -> Word8 -> ReadBuffer -> IO ()
tableSizeUpdate dyntbl w rbuf = do
    let w' = mask5 w
    siz <- decodeI 5 w' rbuf
    suitable <- isSuitableSize siz dyntbl
    unless suitable $ E.throwIO TooLargeTableSize
    renewDynamicTable siz dyntbl

----------------------------------------------------------------

indexed :: DynamicTable -> Word8 -> ReadBuffer -> IO TokenHeader
indexed dyntbl w rbuf = do
    let w' = clearBit w 7
    idx <- decodeI 7 w' rbuf
    entryTokenHeader <$> toIndexedEntry dyntbl idx

incrementalIndexing :: DynamicTable -> Word8 -> ReadBuffer -> IO TokenHeader
incrementalIndexing dyntbl w rbuf = do
    tv@(t, v) <-
        if isIndexedName1 w
            then indexedName dyntbl w rbuf 6 mask6
            else newName dyntbl rbuf
    let e = toEntryToken t v
    insertEntry e dyntbl
    return tv

withoutIndexing :: DynamicTable -> Word8 -> ReadBuffer -> IO TokenHeader
withoutIndexing dyntbl w rbuf
    | isIndexedName2 w = indexedName dyntbl w rbuf 4 mask4
    | otherwise = newName dyntbl rbuf

neverIndexing :: DynamicTable -> Word8 -> ReadBuffer -> IO TokenHeader
neverIndexing dyntbl w rbuf
    | isIndexedName2 w = indexedName dyntbl w rbuf 4 mask4
    | otherwise = newName dyntbl rbuf

----------------------------------------------------------------

indexedName
    :: DynamicTable
    -> Word8
    -> ReadBuffer
    -> Int
    -> (Word8 -> Word8)
    -> IO TokenHeader
indexedName dyntbl w rbuf n mask = do
    let p = mask w
    idx <- decodeI n p rbuf
    t <- entryToken <$> toIndexedEntry dyntbl idx
    val <- decStr (huffmanDecoder dyntbl) rbuf
    let tv = (t, val)
    return tv

newName :: DynamicTable -> ReadBuffer -> IO TokenHeader
newName dyntbl rbuf = do
    let hufdec = huffmanDecoder dyntbl
    t <- toToken <$> decStr hufdec rbuf
    val <- decStr hufdec rbuf
    let tv = (t, val)
    return tv

----------------------------------------------------------------

isHuffman :: Word8 -> Bool
isHuffman w = w `testBit` 7

dropHuffman :: Word8 -> Word8
dropHuffman w = w `clearBit` 7

-- | String decoding (7+) with a temporal Huffman decoder whose buffer is 4096.
decodeString :: ReadBuffer -> IO ByteString
decodeString rbuf = do
    let bufsiz = 4096
    gcbuf <- mallocPlainForeignPtrBytes 4096
    decodeS dropHuffman isHuffman 7 (decodeH gcbuf bufsiz) rbuf

decStr :: HuffmanDecoder -> ReadBuffer -> IO ByteString
decStr = decodeS dropHuffman isHuffman 7

-- | String decoding with Huffman decoder.
decodeS
    :: (Word8 -> Word8)
    -- ^ Dropping prefix and Huffman
    -> (Word8 -> Bool)
    -- ^ Checking Huffman flag
    -> Int
    -- ^ N+
    -> HuffmanDecoder
    -> ReadBuffer
    -> IO ByteString
decodeS mask isH n hufdec rbuf = do
    w <- read8 rbuf
    let p = mask w
        huff = isH w
    len <- decodeI n p rbuf
    if huff
        then hufdec rbuf len
        else extractByteString rbuf len

----------------------------------------------------------------

mask6 :: Word8 -> Word8
mask6 w = w .&. 63

mask5 :: Word8 -> Word8
mask5 w = w .&. 31

mask4 :: Word8 -> Word8
mask4 w = w .&. 15

isIndexedName1 :: Word8 -> Bool
isIndexedName1 w = mask6 w /= 0

isIndexedName2 :: Word8 -> Bool
isIndexedName2 w = mask4 w /= 0

isTableSizeUpdate :: Word8 -> Bool
isTableSizeUpdate w = w .&. 0xe0 == 0x20

----------------------------------------------------------------

-- | Converting a header list of the http-types style to
--   'TokenHeaderList' and 'ValueTable'.
toTokenHeaderTable :: [Header] -> IO TokenHeaderTable
toTokenHeaderTable kvs = do
    arr <- IOA.newArray (minTokenIx, maxTokenIx) Nothing
    tvs <- conv arr
    tbl <- Unsafe.unsafeFreeze arr
    return (tvs, tbl)
  where
    conv :: IOA.IOArray Int (Maybe FieldValue) -> IO TokenHeaderList
    conv arr = go kvs empty
      where
        go :: [Header] -> Builder TokenHeader -> IO TokenHeaderList
        go [] builder = return $ run builder
        go ((k, v) : xs) builder = do
            let t = toToken (foldedCase k)
            unsafeWrite arr (tokenIx t) (Just v)
            let tv = (t, v)
                builder' = builder << tv
            go xs builder'
