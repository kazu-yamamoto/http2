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

-- | Printing 'HeaderSet'.
printHeaderSet :: HeaderSet -> IO ()
printHeaderSet hs = mapM_ printHeader hs
  where
    printHeader (k,v) = do
        BS.putStr $ foldedCase k
        putStr ": "
        BS.putStr v
        putStr "\n"

-- | Merging the emitted header set and the non-emitted header set.
meregeHeaderSet :: HeaderSet -> HeaderSet -> HeaderSet
meregeHeaderSet hdrset notEmitted = reverse $ notEmitted ++ hdrset

-- | Inserting 'Header' to 'HeaderSet'.
insertHeader :: Header -> HeaderSet -> HeaderSet
insertHeader = (:)

