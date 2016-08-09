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
  , entryHeader
  , toEntry
  -- * Getters
  , entrySize
  , entryHeaderName
  , entryHeaderValue
  -- * For initialization
  , dummyEntry
  , maxNumbers
  ) where

import qualified Data.ByteString as BS
import Network.HPACK.Types

----------------------------------------------------------------

-- | Size in bytes.
type Size = Int

-- | Type for table entry. Size includes the 32 bytes magic number.
data Entry = Entry Size Header deriving Show

----------------------------------------------------------------

headerSizeMagicNumber :: Size
headerSizeMagicNumber = 32

headerSize :: Header -> Size
headerSize (k,v) = BS.length k
                 + BS.length v
                 + headerSizeMagicNumber

entryHeader :: Entry -> Header
entryHeader (Entry _ kv) = kv

----------------------------------------------------------------

-- | From 'Header' to 'Entry'.
toEntry :: Header -> Entry
toEntry kv = Entry siz kv
  where
    !siz = headerSize kv

----------------------------------------------------------------

-- | Getting the size of 'Entry'.
entrySize :: Entry -> Size
entrySize (Entry siz _) = siz

-- | Getting 'HeaderName'.
entryHeaderName :: Entry -> HeaderName
entryHeaderName (Entry _ (k,_)) = k

-- | Getting 'HeaderValue'.
entryHeaderValue :: Entry -> HeaderValue
entryHeaderValue (Entry _ (_,v)) = v

----------------------------------------------------------------

-- | Dummy 'Entry' to initialize a dynamic table.
dummyEntry :: Entry
dummyEntry = Entry 0 ("dummyName","dummyValue")

-- | How many entries can be stored in a dynamic table?
maxNumbers :: Size -> Int
maxNumbers siz = siz `div` headerSizeMagicNumber
