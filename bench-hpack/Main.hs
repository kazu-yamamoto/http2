{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Exception
import Criterion.Main
import Network.HPACK
import qualified Network.HPACK2 as N
import Data.ByteString (ByteString)
import Foreign.Marshal.Alloc

main :: IO ()
main = do
    hdrs <- read <$> readFile "bench-hpack/headers.hs"
    hpacks <- prepare hdrs
    _ <- evaluate hpacks
    let bufsiz = 4096
    buf <- mallocBytes bufsiz
    defaultMain [
        bgroup "HPACK encoding" [
              bench "Pure" $ nfIO (enc hdrs)
            , bench "New"  $ nfIO (enc2 hdrs)
            ]
      , bgroup "HPACK decoding" [
              bench "Pure" $ nfIO (dec hpacks)
            , bench "New"  $ nfIO (dec2 hpacks buf bufsiz)
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
        (tbl', frag) <- encodeHeader defaultEncodeStrategy tbl h
        go tbl' hs (b . (frag :))

enc :: [HeaderList] -> IO ()
enc hdrs = do
    tbl <- newDynamicTableForEncoding defaultDynamicTableSize
    go tbl hdrs
  where
    go _    []     = return ()
    go !tbl (h:hs) = do
        (tbl', !_) <- encodeHeader defaultEncodeStrategy tbl h
        go tbl' hs

dec :: [ByteString] -> IO ()
dec hpacks = do
    tbl <- newDynamicTableForDecoding defaultDynamicTableSize
    go tbl hpacks
  where
    go _    []     = return ()
    go !tbl (f:fs) = do
        (tbl', !_) <- decodeHeader tbl f
        go tbl' fs

enc2 :: [HeaderList] -> IO ()
enc2 hdrs = do
    tbl <- N.newDynamicTableForEncoding N.defaultDynamicTableSize
    go tbl hdrs
  where
    go _    []     = return ()
    go !tbl (h:hs) = do
        !_ <- N.encodeHeader N.defaultEncodeStrategy tbl h
        go tbl hs

dec2 :: [ByteString] -> N.Buffer -> N.BufferSize -> IO ()
dec2 hpacks buf siz = do
    tbl <- N.newDynamicTableForDecoding N.defaultDynamicTableSize
    go tbl hpacks
  where
    go _    []     = return ()
    go !tbl (f:fs) = do
        !_ <- N.decodeHeader tbl f buf siz
        go tbl fs
