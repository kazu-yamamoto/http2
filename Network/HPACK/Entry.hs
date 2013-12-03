{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Network.HPACK.Entry (
    toEntry
  , fromEntry
  , entrySize
  , entryHeaderName
  , entryHeaderValue
  , headerSizeMagicNumber
  , dummyEntry
  ) where

import qualified Data.ByteString as BS
import Network.HPACK.Types

headerSizeMagicNumber :: Size
headerSizeMagicNumber = 32

toEntry :: Header -> Entry
toEntry h = (siz,h)
  where
    !siz = headerSize h

headerSize :: Header -> Size
headerSize (k,v) = BS.length k + BS.length v + headerSizeMagicNumber

fromEntry :: Entry -> Header
fromEntry = snd

entrySize :: Entry -> Size
entrySize = fst

entryHeaderName :: Entry -> HeaderName
entryHeaderName (_,(k,_)) = k

entryHeaderValue :: Entry -> HeaderValue
entryHeaderValue (_,(_,v)) = v

dummyEntry :: Entry
dummyEntry = (0,("",""))
