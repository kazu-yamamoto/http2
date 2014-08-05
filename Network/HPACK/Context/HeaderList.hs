{-# LANGUAGE OverloadedStrings #-}

module Network.HPACK.Context.HeaderList where

import qualified Data.ByteString.Char8 as BS
import Network.HPACK.Table

-- | Header list.
type HeaderList = [Header]

-- | Printing 'HeaderList'.
printHeaderList :: HeaderList -> IO ()
printHeaderList hs = mapM_ printHeader hs
  where
    printHeader (k,v) = do
        BS.putStr k
        putStr ": "
        BS.putStr v
        putStr "\n"
