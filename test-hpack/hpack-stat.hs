{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Data.Maybe (fromJust)
import Network.HPACK
import System.Directory
import System.FilePath

import JSON

hdir :: FilePath
hdir = "test-hpack/hpack-test-case/nghttp2"

wdir1 :: FilePath
wdir1 = "test-hpack/hpack-test-case/haskell-http2-naive"

wdir2 :: FilePath
wdir2 = "test-hpack/hpack-test-case/haskell-http2-naive-huffman"

wdir3 :: FilePath
wdir3 = "test-hpack/hpack-test-case/haskell-http2-static"

wdir4 :: FilePath
wdir4 = "test-hpack/hpack-test-case/haskell-http2-static-huffman"

wdir5 :: FilePath
wdir5 = "test-hpack/hpack-test-case/haskell-http2-linear"

wdir6 :: FilePath
wdir6 = "test-hpack/hpack-test-case/haskell-http2-linear-huffman"

main :: IO ()
main = do
    hs <- get getHeaderSize hdir
    hlen <- get getHeaderLen hdir
    ws1 <- get getWireSize wdir1
    ws2 <- get getWireSize wdir2
    ws3 <- get getWireSize wdir3
    ws4 <- get getWireSize wdir4
    ws5 <- get getWireSize wdir5
    ws6 <- get getWireSize wdir6
    let h :: Float = fromIntegral $ sum hs
        w1 :: Float = fromIntegral $ sum ws1
        w2 :: Float = fromIntegral $ sum ws2
        w3 :: Float = fromIntegral $ sum ws3
        w4 :: Float = fromIntegral $ sum ws4
        w5 :: Float = fromIntegral $ sum ws5
        w6 :: Float = fromIntegral $ sum ws6
        hl :: Float = fromIntegral $ sum hlen
    print
        ( w1 / h
        , w2 / h
        , w3 / h
        , w4 / h
        , w5 / h
        , w6 / h
        )
    print ((w4 - w6) / hl)

get :: (FilePath -> IO Int) -> String -> IO [Int]
get func dir = do
    files0 <- valid . sort <$> getDirectoryContents dir
    files1 <- filterM doesFileExist files0
    mapM func files1
  where
    valid = map (dir </>) . filter (isSuffixOf ".json")

getHeaderSize :: FilePath -> IO Int
getHeaderSize file = do
    bs <- BL.readFile file
    let tc = fromJust (decode bs :: Maybe Test)
    let len = sum $ map toT $ cases tc
    return len
  where
    toT (Case _ _ hs _) = sum $ map (\(x, y) -> BS.length (foldedCase x) + BS.length y) hs

getHeaderLen :: FilePath -> IO Int
getHeaderLen file = do
    bs <- BL.readFile file
    let tc = fromJust (decode bs :: Maybe Test)
    let len = length $ cases tc
    return len

getWireSize :: FilePath -> IO Int
getWireSize file = do
    bs <- BL.readFile file
    let tc = fromJust (decode bs :: Maybe Test)
    let len = sum $ map toT $ cases tc
    return len
  where
    toT (Case _ w _ _) = BS.length w `div` 2
