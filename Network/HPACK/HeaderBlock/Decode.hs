{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HPACK.HeaderBlock.Decode (
    decodeHeader,
    decodeTokenHeader,
    ValueTable,
    HeaderTable,
    toHeaderTable,
    getHeaderValue,
    decodeString,
    decodeS,
    decodeSophisticated,
    decodeSimple, -- testing
) where

import Control.Exception (catch, throwIO)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import qualified Data.Array.IO as IOA
import qualified Data.Array.Unsafe as Unsafe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.CaseInsensitive (CI (..))
import Data.Char (isUpper)
import Network.ByteOrder

import Imports hiding (empty)
import Network.HPACK.Builder
import Network.HPACK.HeaderBlock.Integer
import Network.HPACK.Huffman
import Network.HPACK.Table
import Network.HPACK.Types
import Network.HTTP.Semantics.Token

-- | Accessing 'HeaderValue' with 'Token'.
{-# INLINE getHeaderValue #-}
getHeaderValue :: Token -> ValueTable -> Maybe HeaderValue
getHeaderValue t tbl = tbl `unsafeAt` tokenIx t

----------------------------------------------------------------

-- | Converting the HPACK format to 'HeaderList'.
--
--   * Headers are decoded as is.
--   * 'DecodeError' would be thrown if the HPACK format is broken.
--   * 'BufferOverrun' will be thrown if the temporary buffer for Huffman decoding is too small.
decodeHeader
    :: DynamicTable
    -> ByteString
    -- ^ An HPACK format
    -> IO HeaderList
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
--   * 'DecodeError' would be thrown if the HPACK format is broken.
--   * 'BufferOverrun' will be thrown if the temporary buffer for Huffman decoding is too small.
decodeTokenHeader
    :: DynamicTable
    -> ByteString
    -- ^ An HPACK format
    -> IO HeaderTable
decodeTokenHeader dyntbl inp =
    decodeHPACK dyntbl inp (decodeSophisticated (toTokenHeader dyntbl)) `catch` \BufferOverrun -> throwIO HeaderBlockTruncated

decodeHPACK
    :: DynamicTable
    -> ByteString
    -> (ReadBuffer -> IO a)
    -> IO a
decodeHPACK dyntbl inp dec = withReadBuffer inp chkChange
  where
    chkChange rbuf = do
        w <- read8 rbuf
        if isTableSizeUpdate w
            then do
                tableSizeUpdate dyntbl w rbuf
                chkChange rbuf
            else do
                ff rbuf (-1)
                dec rbuf

-- | Converting to 'HeaderList'.
--
--   * Headers are decoded as is.
--   * 'DecodeError' would be thrown if the HPACK format is broken.
--   * 'BufferOverrun' will be thrown if the temporary buffer for Huffman decoding is too small.
decodeSimple
    :: (Word8 -> ReadBuffer -> IO TokenHeader)
    -> ReadBuffer
    -> IO HeaderList
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
                    kvs = map (\(t, v) -> let k = tokenFoldedKey t in (k, v)) tvs
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
--   * 'DecodeError' would be thrown if the HPACK format is broken.
--   * 'BufferOverrun' will be thrown if the temporary buffer for Huffman decoding is too small.
decodeSophisticated
    :: (Word8 -> ReadBuffer -> IO TokenHeader)
    -> ReadBuffer
    -> IO HeaderTable
decodeSophisticated decTokenHeader rbuf = do
    -- using maxTokenIx to reduce condition
    arr <- IOA.newArray (minTokenIx, maxTokenIx) Nothing
    tvs <- pseudoNormal arr
    tbl <- Unsafe.unsafeFreeze arr
    return (tvs, tbl)
  where
    pseudoNormal :: IOA.IOArray Int (Maybe HeaderValue) -> IO TokenHeaderList
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
                            when (isJust mx) $ throwIO IllegalHeaderName
                            -- unknown
                            when (isMaxTokenIx tokenIx) $ throwIO IllegalHeaderName
                            unsafeWrite arr tokenIx (Just v)
                            pseudo
                        else do
                            -- 0-Length Headers Leak - CVE-2019-9516
                            when (tokenKey == "") $ throwIO IllegalHeaderName
                            when (isMaxTokenIx tokenIx && B8.any isUpper (original tokenKey)) $
                                throwIO IllegalHeaderName
                            unsafeWrite arr tokenIx (Just v)
                            if isCookieTokenIx tokenIx
                                then normal 0 empty (empty << v)
                                else normal 0 (empty << tv) empty
                else return []
        normal n builder cookie
            | n > headerLimit = throwIO TooLargeHeader
            | otherwise = do
                leftover <- remainingSize rbuf
                if leftover >= 1
                    then do
                        w <- read8 rbuf
                        tv@(Token{..}, v) <- decTokenHeader w rbuf
                        when isPseudo $ throwIO IllegalHeaderName
                        -- 0-Length Headers Leak - CVE-2019-9516
                        when (tokenKey == "") $ throwIO IllegalHeaderName
                        when (isMaxTokenIx tokenIx && B8.any isUpper (original tokenKey)) $
                            throwIO IllegalHeaderName
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

toTokenHeader :: DynamicTable -> Word8 -> ReadBuffer -> IO TokenHeader
toTokenHeader dyntbl w rbuf
    | w `testBit` 7 = indexed dyntbl w rbuf
    | w `testBit` 6 = incrementalIndexing dyntbl w rbuf
    | w `testBit` 5 = throwIO IllegalTableSizeUpdate
    | w `testBit` 4 = neverIndexing dyntbl w rbuf
    | otherwise = withoutIndexing dyntbl w rbuf

tableSizeUpdate :: DynamicTable -> Word8 -> ReadBuffer -> IO ()
tableSizeUpdate dyntbl w rbuf = do
    let w' = mask5 w
    siz <- decodeI 5 w' rbuf
    suitable <- isSuitableSize siz dyntbl
    unless suitable $ throwIO TooLargeTableSize
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
toHeaderTable :: [(CI HeaderName, HeaderValue)] -> IO HeaderTable
toHeaderTable kvs = do
    arr <- IOA.newArray (minTokenIx, maxTokenIx) Nothing
    tvs <- conv arr
    tbl <- Unsafe.unsafeFreeze arr
    return (tvs, tbl)
  where
    conv :: IOA.IOArray Int (Maybe HeaderValue) -> IO TokenHeaderList
    conv arr = go kvs empty
      where
        go
            :: [(CI HeaderName, HeaderValue)] -> Builder TokenHeader -> IO TokenHeaderList
        go [] builder = return $ run builder
        go ((k, v) : xs) builder = do
            let t = toToken (foldedCase k)
            unsafeWrite arr (tokenIx t) (Just v)
            let tv = (t, v)
                builder' = builder << tv
            go xs builder'
