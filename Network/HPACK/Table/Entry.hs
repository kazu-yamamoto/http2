{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Network.HPACK.Table.Entry (
  -- * Type
    Size
  , Entry
  , Header      -- re-exporting
  , HeaderName  -- re-exporting
  , HeaderValue -- re-exporting
  , Index       -- re-exporting
  -- * Header and Entry
  , toEntry
  , fromEntry
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
type Entry = (Size,Header)

----------------------------------------------------------------

headerSizeMagicNumber :: Size
headerSizeMagicNumber = 32

headerSize :: Header -> Size
headerSize (k,v) = BS.length (fromHeaderName k)
                 + BS.length v
                 + headerSizeMagicNumber

----------------------------------------------------------------

-- | From 'Header' to 'Entry'.
toEntry :: Header -> Entry
toEntry h = (siz,h)
  where
    !siz = headerSize h

-- | From 'Entry' to 'Header'.
fromEntry :: Entry -> Header
fromEntry = snd

----------------------------------------------------------------

-- | Getting the size of 'Entry'.
entrySize :: Entry -> Size
entrySize = fst

-- | Getting 'HeaderName'.
entryHeaderName :: Entry -> HeaderName
entryHeaderName (_,(k,_)) = k

-- | Getting 'HeaderValue'.
entryHeaderValue :: Entry -> HeaderValue
entryHeaderValue (_,(_,v)) = v

----------------------------------------------------------------

-- | Dummy 'Entry' to initialize a table.
dummyEntry :: Entry
dummyEntry = (0,("dummy","dummy"))

maxNumbers :: Size -> Int
maxNumbers siz = siz `div` headerSizeMagicNumber
