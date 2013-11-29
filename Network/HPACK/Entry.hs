{-# LANGUAGE BangPatterns #-}

module Network.HPACK.Entry (
    toEntry
  , fromEntry
  , entrySize
  , entryHeaderName
  ) where

import qualified Data.ByteString as BS
import Network.HPACK.Types

toEntry :: Header -> Entry
toEntry h = (siz,h)
  where
    !siz = headerSize h

headerSize :: Header -> Size
headerSize (k,v) = BS.length k + BS.length v + 32

fromEntry :: Entry -> Header
fromEntry = snd

entrySize :: Entry -> Size
entrySize = fst

entryHeaderName :: Entry -> HeaderName
entryHeaderName (_,(k,_)) = k
