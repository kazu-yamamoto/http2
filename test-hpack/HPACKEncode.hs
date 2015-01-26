{-# LANGUAGE OverloadedStrings #-}

module HPACKEncode (
    run
  , EncodeStrategy(..)
  , defaultEncodeStrategy
  , CompressionAlgo(..)
  ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Hex
import Network.HPACK
import Network.HPACK.Table

import JSON

data Conf = Conf {
    debug :: Bool
  , enc :: HPACKEncoding
  }

run :: Bool -> EncodeStrategy -> Test -> IO [ByteString]
run _ _    (Test _        _ [])        = return []
run d stgy (Test _ _ ccs@(c:_)) = do
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
    (ehdrtbl',hx) <- test conf c ehdrtbl
    testLoop conf cs ehdrtbl' (hx:hxs)

test :: Conf
     -> Case
     -> DynamicTable
     -> IO (DynamicTable, ByteString)
test conf c ehdrtbl = do
    (ehdrtbl',out) <- enc conf ehdrtbl hs
    let hex' = hex out
    when (debug conf) $ do
        putStrLn "---- Output context"
        printDynamicTable ehdrtbl'
        putStrLn "--------------------------------"
    return (ehdrtbl', hex')
  where
    hs = headers c
