{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Exception
import Criterion.Main
import Network.HPACK

main :: IO ()
main = do
    hdrs <- read <$> readFile "bench-hpack/headers.hs"
    _ <- evaluate hdrs
    defaultMain [
        bgroup "HPACK encoding" [
              bench "Pure" $ nfIO (enc hdrs)
            ]
      ]

----------------------------------------------------------------

enc :: [HeaderList] -> IO ()
enc hdrs = do
    tbl <- newDynamicTableForEncoding defaultDynamicTableSize
    go tbl hdrs
  where
    go _    []     = return ()
    go !tbl (h:hs) = do
        (tbl', !_) <- encodeHeader defaultEncodeStrategy tbl h
        go tbl' hs

