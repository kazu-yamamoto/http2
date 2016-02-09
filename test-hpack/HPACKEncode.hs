{-# LANGUAGE OverloadedStrings #-}

module HPACKEncode (
    run
  , EncodeStrategy(..)
  , defaultEncodeStrategy
  , CompressionAlgo(..)
  ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Char
import Data.Hex
import Network.HPACK
import Network.HPACK.Table

import JSON

data Conf = Conf {
    debug :: Bool
  , enc :: HPACKEncoding
  }

run :: Bool -> EncodeStrategy -> Test -> IO [ByteString]
run _ _    (Test        _ []) = return []
run d stgy (Test _ ccs@(c:_)) = do
    let siz = maybe 4096 id $ size c
    ehdrtbl <- newDynamicTableForEncoding siz
    let conf = Conf { debug = d, enc = encodeHeader stgy }
    testLoop conf ccs ehdrtbl []

testLoop :: Conf
         -> [Case]
         -> DynamicTable
         -> [ByteString]
         -> IO [ByteString]
testLoop _    []     _    hexs = return $ reverse hexs
testLoop conf (c:cs) ehdrtbl hxs = do
    hx <- test conf c ehdrtbl
    testLoop conf cs ehdrtbl (C8.map toLower hx : hxs)

test :: Conf
     -> Case
     -> DynamicTable
     -> IO ByteString
test conf c ehdrtbl = do
    out <- enc conf ehdrtbl hs
    let hex' = hex out
    when (debug conf) $ do
        putStrLn "---- Output context"
        printDynamicTable ehdrtbl
        putStrLn "--------------------------------"
    return hex'
  where
    hs = headers c
