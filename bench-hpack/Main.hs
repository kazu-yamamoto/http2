{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Exception
import Criterion.Main
import Network.HPACK
import Data.ByteString (ByteString)

----------------------------------------------------------------

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
              bench "LinearH" $ nfIO (enc hdrs)
            ]
      ]

----------------------------------------------------------------
prepare :: [[Header]] -> IO [ByteString]
prepare hdrs = do
    tbl <- newContextForEncoding 4096
    go tbl hdrs id
  where
    go _    []     b = return (b [])
    go !tbl (h:hs) b = do
        (tbl',frag) <- encodeRequestHeader defaultEncodeStrategy tbl h
        go tbl' hs (b . (frag :))

dec :: [ByteString] -> IO ()
dec hpacks = do
    tbl <- newContextForDecoding 4096
    go tbl hpacks
  where
    go _    []     = return ()
    go !tbl (f:fs) = do
        (tbl',!_) <- decodeRequestHeader tbl f
        go tbl' fs

enc :: [[Header]] -> IO ()
enc hdrs = do
    tbl <- newContextForEncoding 4096
    go tbl hdrs
  where
    go _    []     = return ()
    go !tbl (h:hs) = do
        (tbl',!_) <- encodeRequestHeader defaultEncodeStrategy tbl h
        go tbl' hs
