{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Exception
import Criterion.Main
import Network.HPACK
import Data.ByteString (ByteString)

main :: IO ()
main = do
    hdrs <- read <$> readFile "bench-hpack/headers.hs"
    hpacks <- prepare hdrs
    _ <- evaluate hpacks
    defaultMain [
        bgroup "HPACK encoding" [
              bench "New"  $ nfIO (enc hdrs)
            ]
      , bgroup "HPACK decoding" [
              bench "New"  $ nfIO (dec hpacks)
            ]
      ]

----------------------------------------------------------------
prepare :: [HeaderList] -> IO [ByteString]
prepare hdrs = do
    tbl <- newDynamicTableForEncoding defaultDynamicTableSize
    go tbl hdrs id
  where
    go _    []     b = return (b [])
    go !tbl (h:hs) b = do
        !frag <- encodeHeader defaultEncodeStrategy 4096 tbl h
        go tbl hs (b . (frag :))

enc :: [HeaderList] -> IO ()
enc hdrs = do
    tbl <- newDynamicTableForEncoding defaultDynamicTableSize
    go tbl hdrs
  where
    go _    []     = return ()
    go !tbl (h:hs) = do
        !_ <- encodeHeader defaultEncodeStrategy { compressionAlgo = Linear, useHuffman = True } 4096 tbl h
        go tbl hs

dec :: [ByteString] -> IO ()
dec hpacks = do
    tbl <- newDynamicTableForDecoding defaultDynamicTableSize 4096
    go tbl hpacks
  where
    go _    []     = return ()
    go !tbl (f:fs) = do
        !_ <- decodeHeader tbl f
        go tbl fs
