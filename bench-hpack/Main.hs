{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Exception
import Criterion.Main
import Network.HPACK
import Data.ByteString (ByteString)

----------------------------------------------------------------

naive, naiveh, static, statich, linear, linearh :: EncodeStrategy
naive   = EncodeStrategy {compressionAlgo = Naive,  useHuffman = False}
naiveh  = EncodeStrategy {compressionAlgo = Naive,  useHuffman = True}
static  = EncodeStrategy {compressionAlgo = Static, useHuffman = False}
statich = EncodeStrategy {compressionAlgo = Static, useHuffman = True}
linear  = EncodeStrategy {compressionAlgo = Linear, useHuffman = False}
linearh = EncodeStrategy {compressionAlgo = Linear, useHuffman = True}

main :: IO ()
main = do
    hdrs <- read <$> readFile "bench-hpack/headers.hs"
    hpacks <- prepare hdrs
    _ <- evaluate hpacks
    defaultMain [
        bgroup "HPACK decoding" [
              bench "LinearH" $ nfIO (dec hpacks)
            ]
      , bgroup "HPACK encoding" [
              bench "Naive"   $ nfIO (enc naive   hdrs)
            , bench "HaiveH"  $ nfIO (enc naiveh  hdrs)
            , bench "Static"  $ nfIO (enc static  hdrs)
            , bench "StaticH" $ nfIO (enc statich hdrs)
            , bench "Linear"  $ nfIO (enc linear  hdrs)
            , bench "LinearH" $ nfIO (enc linearh hdrs)
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
        !frag <- encodeHeader linearh 4096 tbl h
        go tbl hs (b . (frag :))

dec :: [ByteString] -> IO ()
dec hpacks = do
    tbl <- newDynamicTableForDecoding defaultDynamicTableSize 4096
    go tbl hpacks
  where
    go _    []     = return ()
    go !tbl (f:fs) = do
        !_ <- decodeHeader tbl f
        go tbl fs

enc :: EncodeStrategy -> [HeaderList] -> IO ()
enc stgy hdrs = do
    tbl <- newDynamicTableForEncoding defaultDynamicTableSize
    go tbl hdrs
  where
    go _    []     = return ()
    go !tbl (h:hs) = do
        !_ <- encodeHeader stgy 4096 tbl h
        go tbl hs
