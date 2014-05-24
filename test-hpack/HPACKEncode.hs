{-# LANGUAGE OverloadedStrings #-}

module HPACKEncode (
    run
  , EncodeStrategy(..)
  , defaultEncodeStrategy
  , CompressionAlgo(..)
  ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Network.HPACK
import Network.HPACK.Context

import HexString
import Types

data Conf = Conf {
    debug :: Bool
  , enc :: HPACKEncoding
  }

run :: Bool -> EncodeStrategy -> Test -> IO [ByteString]
run _ _    (Test _        _ [])        = return []
run d stgy (Test _ _ ccs@(c:_)) = do
    let siz = maybe 4096 id $ size c
    ectx <- newContextForEncoding siz
    let conf = Conf { debug = d, enc = encodeHeader stgy }
    testLoop conf ccs ectx []

testLoop :: Conf
         -> [Case]
         -> Context
         -> [ByteString]
         -> IO [ByteString]
testLoop _    []     _    hexs = return $ reverse hexs
testLoop conf (c:cs) ectx hexs = do
    (ectx',hex) <- test conf c ectx
    testLoop conf cs ectx' (hex:hexs)

test :: Conf
     -> Case
     -> Context
     -> IO (Context, ByteString)
test conf c ectx = do
    (ectx',out) <- enc conf ectx hs
    let hex' = toHexString out
    when (debug conf) $ do
        putStrLn "---- Output context"
        printContext ectx'
        putStrLn "--------------------------------"
    return (ectx', hex')
  where
    hs = headers c