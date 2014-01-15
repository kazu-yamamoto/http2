{-# LANGUAGE OverloadedStrings #-}

module HPACK (run, Result(..)) where

import Data.ByteString (ByteString)
import Control.Exception
import qualified Data.ByteString.Char8 as B8
import Data.List (sort)
import Network.HPACK
import Network.HPACK.HeaderBlock.Decode
import Network.HPACK.Huffman

import HexString
import Types

data Result = Pass [ByteString] | Fail String deriving (Eq,Show)

run :: Test -> IO Result
run (Test _ reqOrRsp _ cs) = do
    dctx <- newContext 4096 -- FIXME
    ectx <- newContext 4096 -- FIXME
    let stgy = defaultEncodeStrategy
    let (dec,enc) = case reqOrRsp of
            "request" -> (decodeRequestHeader,  encodeRequestHeader  stgy)
            _         -> (decodeResponseHeader, encodeResponseHeader stgy)
    testLoop cs dec dctx enc ectx []

testLoop :: [Case]
         -> HPACKDecoding -> Context
         -> HPACKEncoding -> Context
         -> [ByteString]
         -> IO Result
testLoop []     _   _    _   _    hexs = return $ Pass $ reverse hexs
testLoop (c:cs) dec dctx enc ectx hexs = do
    res <- test c dec dctx enc ectx
    case res of
        Right (dctx', ectx', hex) -> testLoop cs dec dctx' enc ectx' (hex:hexs)
        Left e                    -> return $ Fail e

test :: Case
     -> HPACKDecoding -> Context
     -> HPACKEncoding -> Context
     -> IO (Either String (Context, Context, ByteString))
test c dec dctx enc ectx = do
    x <- try $ dec dctx inp
    case x of
        Left e -> return $ Left $ show (e :: DecodeError)
        Right (dctx',hs') -> do
            (ectx',out) <- enc ectx hs
            let pass = sort hs == sort hs'
                hex' = toHexString out
            if pass then
                return $ Right (dctx', ectx', hex')
              else
                return $ Left $ "Headers are different in " ++ B8.unpack hex ++ ":\n" ++ show hd ++ "\n" ++ show hs ++ "\n" ++ show hs'
  where
    hex = wire c
    inp = fromHexString hex
    hs = headers c
    hd = fromByteStream huffmanDecodeInRequest inp
