{-# LANGUAGE OverloadedStrings #-}

module HPACK (run
           , Result(..)
           , EncodeStrategy(..)
           , defaultEncodeStrategy
           , CompressionAlgo(..)
           ) where

import Control.Exception
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.List (sort)
import Network.HPACK
import Network.HPACK.Context
import Network.HPACK.Context.HeaderSet
import Network.HPACK.HeaderBlock
import Network.HPACK.Huffman

import HexString
import Types

data Conf = Conf {
    debug :: Bool
  , enc :: HPACKEncoding
  , dec :: HPACKDecoding
  , hbk :: ByteStream -> Either DecodeError HeaderBlock
  }

data Result = Pass [ByteString] | Fail String deriving (Eq,Show)

run :: Bool -> EncodeStrategy -> Test -> IO Result
run _ _    (Test _ _        _ [])        = return $ Pass []
run d stgy (Test _ reqOrRsp _ ccs@(c:_)) = do
    let siz = size c
    dctx <- newContextForDecoding siz
    ectx <- newContextForEncoding siz
    let conf
          | reqOrRsp == "request" = Conf {
                debug = d
              , enc = encodeRequestHeader stgy
              , dec = decodeRequestHeader
              , hbk = fromByteStream huffmanDecodeInRequest
              }
          | otherwise = Conf {
                debug = d
              , enc = encodeResponseHeader stgy
              , dec = decodeResponseHeader
              , hbk = fromByteStream huffmanDecodeInResponse
              }
    testLoop conf ccs dctx ectx []

testLoop :: Conf
         -> [Case]
         -> Context
         -> Context
         -> [ByteString]
         -> IO Result
testLoop _    []     _    _    hexs = return $ Pass $ reverse hexs
testLoop conf (c:cs) dctx ectx hexs = do
    res <- test conf c dctx ectx
    case res of
        Right (dctx', ectx', hex) -> testLoop conf cs dctx' ectx' (hex:hexs)
        Left e                    -> return $ Fail e

test :: Conf
     -> Case
     -> Context
     -> Context
     -> IO (Either String (Context, Context, ByteString))
test conf c dctx ectx = do
    -- context is destructive!!!
    when (debug conf) $ do
        putStrLn "--------------------------------"
        putStrLn "---- Input headerset"
        printHeaderSet $ sort hs
        putStrLn "---- Input context"
        printContext dctx
        putStrLn "---- Input Hex"
        B8.putStrLn hex
        putStrLn "---- Input header block"
        print hd
    x <- try $ dec conf dctx inp
    case x of
        Left e -> return $ Left $ show (e :: DecodeError)
        Right (dctx',hs') -> do
            -- This is for hpack-encoding only.
            (ectx',out) <- enc conf ectx hs
            let pass = sort hs == sort hs'
                hex' = toHexString out
            when (debug conf) $ do
                putStrLn "---- Output headerset"
                printHeaderSet $ sort hs'
                putStrLn "---- Output context"
                printContext dctx'
                putStrLn "--------------------------------"
            if pass then
                return $ Right (dctx', ectx', hex')
              else
                return $ Left $ "Headers are different in " ++ B8.unpack hex ++ ":\n" ++ show hd ++ "\n" ++ show hs ++ "\n" ++ show hs'
  where
    hex = wire c
    inp = fromHexString hex
    hs = headers c
    hd = hbk conf inp
