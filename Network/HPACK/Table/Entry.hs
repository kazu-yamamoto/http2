{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Network.HPACK.Table.Entry (
  -- * Type
    Size
  , Entry(..)
  , Header      -- re-exporting
  , HeaderName  -- re-exporting
  , HeaderValue -- re-exporting
  , Index       -- re-exporting
  -- * Header and Entry
  , toEntry
  , toEntryToken
  , fromEntry
  -- * Getters
  , entrySize
  , entryHeader
  , entryHeaderName
  , entryHeaderValue
  -- * For initialization
  , dummyEntry
  , maxNumbers
  ) where

import qualified Data.ByteString as BS
import Network.HPACK.Token
import Network.HPACK.Types

----------------------------------------------------------------

-- | Size in bytes.
type Size = Int

-- | Type for table entry. Size includes the 32 bytes magic number.
data Entry = Entry Size Token Header deriving Show

----------------------------------------------------------------

headerSizeMagicNumber :: Size
headerSizeMagicNumber = 32

headerSize :: Header -> Size
headerSize (k,v) = BS.length k
                 + BS.length v
                 + headerSizeMagicNumber

----------------------------------------------------------------

-- | From 'Header' to 'Entry'.
toEntry :: Header -> Entry
toEntry kv = Entry siz tokenDummy kv
  where
    !siz = headerSize kv

toEntryToken :: Header -> Token -> Entry
toEntryToken kv t = Entry siz t kv
  where
    !siz = headerSize kv

-- | From 'Entry' to 'Header'.
fromEntry :: Entry -> Header
fromEntry (Entry _ _ kv) = kv

----------------------------------------------------------------

-- | Getting the size of 'Entry'.
entrySize :: Entry -> Size
entrySize (Entry siz _ _) = siz

-- | Getting 'Header'.
entryHeader :: Entry -> Header
entryHeader (Entry _ _ kv) = kv

-- | Getting 'HeaderName'.
entryHeaderName :: Entry -> HeaderName
entryHeaderName (Entry _ _ (k,_)) = k

-- | Getting 'HeaderValue'.
entryHeaderValue :: Entry -> HeaderValue
entryHeaderValue (Entry _ _ (_,v)) = v

----------------------------------------------------------------

-- | Dummy 'Entry' to initialize a dynamic table.
dummyEntry :: Entry
dummyEntry = Entry 0 tokenDummy ("dummy","dummy")

-- | How many entries can be stored in a dynamic table?
maxNumbers :: Size -> Int
maxNumbers siz = siz `div` headerSizeMagicNumber
