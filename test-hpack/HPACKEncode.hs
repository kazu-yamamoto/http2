{-# LANGUAGE OverloadedStrings #-}

module HPACKEncode (
    run,
    EncodeStrategy (..),
    defaultEncodeStrategy,
    CompressionAlgo (..),
) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.Char
import Data.Maybe (fromMaybe)
import Network.HPACK
import Network.HPACK.Table

import JSON

data Conf = Conf
    { debug :: Bool
    , enc :: DynamicTable -> HeaderList -> IO ByteString
    }

run :: Bool -> EncodeStrategy -> Test -> IO [ByteString]
run _ _ (Test _ []) = return []
run d stgy (Test _ ccs@(c : _)) = do
    let siz = fromMaybe 4096 $ size c
    withDynamicTableForEncoding siz $ \dyntbl -> do
        let conf = Conf{debug = d, enc = encodeHeader stgy 4096}
        testLoop conf ccs dyntbl []

testLoop
    :: Conf
    -> [Case]
    -> DynamicTable
    -> [ByteString]
    -> IO [ByteString]
testLoop _ [] _ hexs = return $ reverse hexs
testLoop conf (c : cs) dyntbl hxs = do
    hx <- test conf c dyntbl
    testLoop conf cs dyntbl (C8.map toLower hx : hxs)

test
    :: Conf
    -> Case
    -> DynamicTable
    -> IO ByteString
test conf c dyntbl = do
    out <- enc conf dyntbl hs
    let hex' = B16.encode out
    when (debug conf) $ do
        putStrLn "---- Output context"
        printDynamicTable dyntbl
        putStrLn "--------------------------------"
    return hex'
  where
    hs = headers c
