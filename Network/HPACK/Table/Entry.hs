{-# LANGUAGE OverloadedStrings #-}

module Network.HPACK.Table.Entry (
    -- * Type
    Size,
    Entry (..),
    FieldValue, -- re-exporting
    Index, -- re-exporting

    -- * Header and Entry
    toEntry,
    toEntryToken,

    -- * Getters
    entrySize,
    entryTokenHeader,
    entryToken,
    entryHeaderName,
    entryFieldValue,

    -- * For initialization
    dummyEntry,
    maxNumbers,
) where

import qualified Data.ByteString as BS
import Network.HPACK.Types
import Network.HTTP.Semantics

import Imports

----------------------------------------------------------------

-- | Size in bytes.
type Size = Int

-- | Type for table entry. Size includes the 32 bytes magic number.
data Entry = Entry Size Token FieldValue deriving (Show)

----------------------------------------------------------------

headerSizeMagicNumber :: Size
headerSizeMagicNumber = 32

headerSize :: Header -> Size
headerSize (k, v) =
    BS.length (foldedCase k)
        + BS.length v
        + headerSizeMagicNumber

headerSize' :: Token -> FieldValue -> Size
headerSize' t v =
    BS.length (tokenFoldedKey t)
        + BS.length v
        + headerSizeMagicNumber

----------------------------------------------------------------

-- | From 'Header' to 'Entry'.
toEntry :: Header -> Entry
toEntry kv@(k, v) = Entry siz t v
  where
    t = toToken $ foldedCase k
    siz = headerSize kv

toEntryToken :: Token -> FieldValue -> Entry
toEntryToken t v = Entry siz t v
  where
    siz = headerSize' t v

----------------------------------------------------------------

-- | Getting the size of 'Entry'.
entrySize :: Entry -> Size
entrySize (Entry siz _ _) = siz

-- | Getting 'TokenHeader'.
entryTokenHeader :: Entry -> TokenHeader
entryTokenHeader (Entry _ t v) = (t, v)

-- | Getting 'Token'.
entryToken :: Entry -> Token
entryToken (Entry _ t _) = t

-- | Getting 'HeaderName'.
entryHeaderName :: Entry -> HeaderName
entryHeaderName (Entry _ t _) = tokenKey t -- xxx

-- | Getting 'FieldValue'.
entryFieldValue :: Entry -> FieldValue
entryFieldValue (Entry _ _ v) = v

----------------------------------------------------------------

-- | Dummy 'Entry' to initialize a dynamic table.
dummyEntry :: Entry
dummyEntry = Entry 0 tokenMax "dummyValue"

-- | How many entries can be stored in a dynamic table?
maxNumbers :: Size -> Int
maxNumbers siz = siz `div` headerSizeMagicNumber
