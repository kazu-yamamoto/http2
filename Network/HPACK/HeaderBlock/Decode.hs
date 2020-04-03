{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Network.HPACK.HeaderBlock.Decode (
    decodeHeader
  , decodeTokenHeader
  , ValueTable
  , HeaderTable
  , toHeaderTable
  , getHeaderValue
  , decodeString
  , decodeS
  ) where

import Control.Exception (throwIO, catch)
import Data.Array (Array)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import qualified Data.Array.IO as IOA
import qualified Data.Array.Unsafe as Unsafe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Char (isUpper)
import Data.CaseInsensitive (CI(..))
import Network.ByteOrder

import Imports hiding (empty)
import Network.HPACK.Builder
import Network.HPACK.HeaderBlock.Integer
import Network.HPACK.Huffman
import Network.HPACK.Table
import Network.HPACK.Token
import Network.HPACK.Types

-- | An array to get 'HeaderValue' quickly.
--   'getHeaderValue' should be used.
--   Internally, the key is 'Token' 'ix'.
type ValueTable = Array Int (Maybe HeaderValue)

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
decodeHeader :: DynamicTable
             -> ByteString -- ^ An HPACK format
             -> IO HeaderList
decodeHeader dyntbl inp = decodeHPACK dyntbl inp decodeSimple

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
decodeTokenHeader :: DynamicTable
                  -> ByteString -- ^ An HPACK format
                  -> IO HeaderTable
decodeTokenHeader dyntbl inp =
    decodeHPACK dyntbl inp decodeSophisticated `catch` \BufferOverrun -> throwIO HeaderBlockTruncated

decodeHPACK :: DynamicTable
            -> ByteString
            -> (DynamicTable -> ReadBuffer -> IO a)
            -> IO a
decodeHPACK dyntbl inp dec = withReadBuffer inp chkChange
  where
    chkChange rbuf = do
        w <- read8 rbuf
        if isTableSizeUpdate w then do
            tableSizeUpdate dyntbl w rbuf
            chkChange rbuf
          else do
            ff rbuf (-1)
            dec dyntbl rbuf

decodeSimple :: DynamicTable -> ReadBuffer -> IO HeaderList
decodeSimple dyntbl rbuf = go empty
  where
    go builder = do
        leftover <- remainingSize rbuf
        if leftover >= 1 then do
            w <- read8 rbuf
            tv <- toTokenHeader dyntbl w rbuf
            let builder' = builder << tv
            go builder'
          else do
            let tvs = run builder
                kvs = map (\(t,v) -> let k = tokenFoldedKey t in (k,v)) tvs
            return kvs

decodeSophisticated :: DynamicTable -> ReadBuffer
                    -> IO HeaderTable
decodeSophisticated dyntbl rbuf = do
    -- using maxTokenIx to reduce condition
    arr <- IOA.newArray (minTokenIx,maxTokenIx) Nothing
    tvs <- pseudoNormal arr
    tbl <- Unsafe.unsafeFreeze arr
    return (tvs, tbl)
  where
    pseudoNormal :: IOA.IOArray Int (Maybe HeaderValue) -> IO TokenHeaderList
    pseudoNormal arr = pseudo
      where
        pseudo = do
            leftover <- remainingSize rbuf
            if leftover >= 1 then do
                w <- read8 rbuf
                tv@(Token{..},v) <- toTokenHeader dyntbl w rbuf
                if isPseudo then do
                    mx <- unsafeRead arr tokenIx
                    when (isJust mx) $ throwIO IllegalHeaderName
                    when (isMaxTokenIx tokenIx) $ throwIO IllegalHeaderName
                    unsafeWrite arr tokenIx (Just v)
                    pseudo
                  else do
                    when (isMaxTokenIx tokenIx && B8.any isUpper (original tokenKey)) $
                        throwIO IllegalHeaderName
                    unsafeWrite arr tokenIx (Just v)
                    if isCookieTokenIx tokenIx then
                        normal empty (empty << v)
                      else
                        normal (empty << tv) empty
              else
                return []
        normal builder cookie = do
            leftover <- remainingSize rbuf
            if leftover >= 1 then do
                w <- read8 rbuf
                tv@(Token{..},v) <- toTokenHeader dyntbl w rbuf
                when isPseudo $ throwIO IllegalHeaderName
                when (isMaxTokenIx tokenIx && B8.any isUpper (original tokenKey)) $
                    throwIO IllegalHeaderName
                unsafeWrite arr tokenIx (Just v)
                if isCookieTokenIx tokenIx then
                    normal builder (cookie << v)
                  else
                    normal (builder << tv) cookie
              else do
                let tvs0 = run builder
                    cook = run cookie
                if null cook then
                    return tvs0
                  else do
                    let v = BS.intercalate "; " cook
                        tvs = (tokenCookie, v) : tvs0
                    unsafeWrite arr cookieTokenIx (Just v)
                    return tvs

toTokenHeader :: DynamicTable -> Word8 -> ReadBuffer -> IO TokenHeader
toTokenHeader dyntbl w rbuf
  | w `testBit` 7 = indexed             dyntbl w rbuf
  | w `testBit` 6 = incrementalIndexing dyntbl w rbuf
  | w `testBit` 5 = throwIO IllegalTableSizeUpdate
  | w `testBit` 4 = neverIndexing       dyntbl w rbuf
  | otherwise     = withoutIndexing     dyntbl w rbuf

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
    tv@(t,v) <- if isIndexedName1 w then
                    indexedName dyntbl w rbuf 6 mask6
                else
                    newName dyntbl rbuf
    let e = toEntryToken t v
    insertEntry e dyntbl
    return tv

withoutIndexing :: DynamicTable -> Word8 -> ReadBuffer -> IO TokenHeader
withoutIndexing dyntbl w rbuf
  | isIndexedName2 w = indexedName dyntbl w rbuf 4 mask4
  | otherwise        = newName dyntbl rbuf

neverIndexing :: DynamicTable -> Word8 -> ReadBuffer -> IO TokenHeader
neverIndexing dyntbl w rbuf
  | isIndexedName2 w = indexedName dyntbl w rbuf 4 mask4
  | otherwise        = newName dyntbl rbuf

----------------------------------------------------------------

indexedName :: DynamicTable -> Word8 -> ReadBuffer
            -> Int -> (Word8 -> Word8)
            -> IO TokenHeader
indexedName dyntbl w rbuf n mask = do
    let p = mask w
    idx <- decodeI n p rbuf
    t <- entryToken <$> toIndexedEntry dyntbl idx
    val <- decStr (huffmanDecoder dyntbl) rbuf
    let tv = (t,val)
    return tv

newName :: DynamicTable -> ReadBuffer -> IO TokenHeader
newName dyntbl rbuf = do
    let hufdec = huffmanDecoder dyntbl
    t <- toToken <$> decStr hufdec rbuf
    val <- decStr hufdec rbuf
    let tv = (t,val)
    return tv

----------------------------------------------------------------

isHuffman :: Word8 -> Bool
isHuffman w = w `testBit` 7

dropHuffman :: Word8 -> Word8
dropHuffman w = w `clearBit` 7

-- | String decoding (7+) with a temporal Huffman decoder whose buffer is 4096.
decodeString :: ReadBuffer -> IO ByteString
decodeString rbuf = snd <$> withWriteBuffer' 4096
  (\wbuf -> decodeS dropHuffman isHuffman (decodeH wbuf) rbuf)

decStr :: HuffmanDecoder -> ReadBuffer -> IO ByteString
decStr = decodeS dropHuffman isHuffman

-- | String decoding with Huffman decoder.
decodeS :: (Word8 -> Word8) -- ^ Dropping prefix
        -> (Word8 -> Bool)  -- ^ Checking Huffman flag
        -> HuffmanDecoder
        -> ReadBuffer
        -> IO ByteString
decodeS mask isH hufdec rbuf = do
    w <- read8 rbuf
    let p = mask w
        huff = isH w
    len <- decodeI 7 p rbuf
    if huff then
        hufdec rbuf len
      else
        extractByteString rbuf len

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

-- | A pair of token list and value table.
type HeaderTable = (TokenHeaderList, ValueTable)

-- | Converting a header list of the http-types style to
--   'TokenHeaderList' and 'ValueTable'.
toHeaderTable :: [(CI HeaderName,HeaderValue)]  -> IO HeaderTable
toHeaderTable kvs = do
    arr <- IOA.newArray (minTokenIx,maxTokenIx) Nothing
    tvs <- conv arr
    tbl <- Unsafe.unsafeFreeze arr
    return (tvs, tbl)
  where
    conv :: IOA.IOArray Int (Maybe HeaderValue) -> IO TokenHeaderList
    conv arr = go kvs empty
      where
        go :: [(CI HeaderName,HeaderValue)] -> Builder TokenHeader -> IO TokenHeaderList
        go []         builder = return $ run builder
        go ((k,v):xs) builder = do
            let t = toToken (foldedCase k)
            unsafeWrite arr (tokenIx t) (Just v)
            let tv = (t,v)
                builder' = builder << tv
            go xs builder'
