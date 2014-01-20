{-# LANGUAGE OverloadedStrings #-}

module Network.HPACK.Context.HeaderSet where

import qualified Data.ByteString.Char8 as BS
import Network.HPACK.Table

-- | Header set.
type HeaderSet = [Header]

-- | Printing 'HeaderSet'.
printHeaderSet :: HeaderSet -> IO ()
printHeaderSet hs = mapM_ printHeader hs
  where
    printHeader (k,v) = do
        BS.putStr k
        putStr ": "
        BS.putStr v
        putStr "\n"
