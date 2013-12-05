{-# LANGUAGE OverloadedStrings #-}

module Network.HPACK.Context.HeaderSet where

import Data.CaseInsensitive (foldedCase)
import qualified Data.ByteString.Char8 as BS
import Network.HPACK.Table

-- | Header set.
type HeaderSet = [Header]

-- | Empty header set.
emptyHeaderSet :: HeaderSet
emptyHeaderSet = []

-- FIXME: should use BlazeBuilder
-- | Converting 'HeaderSet' to 'String.
showHeaderSet :: HeaderSet -> String
showHeaderSet hs = BS.unpack . BS.concat $ map showHeader hs
  where
    showHeader (k,v) = BS.concat [ foldedCase k
                                 , ": "
                                 , v
                                 , "\n"
                                 ]

-- | Merging the emitted header set and the non-emitted header set.
meregeHeaderSet :: HeaderSet -> HeaderSet -> HeaderSet
meregeHeaderSet hdrset notEmitted = reverse $ notEmitted ++ hdrset

-- | Inserting 'Header' to 'HeaderSet'.
insertHeader :: Header -> HeaderSet -> HeaderSet
insertHeader = (:)

