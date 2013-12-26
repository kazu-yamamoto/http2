{-# LANGUAGE OverloadedStrings #-}

module HPACK (run, Result(..)) where

import Control.Exception
import qualified Data.ByteString as BS
import Data.List (sort)
import Network.HPACK

import HexString
import Types

data Result = Pass | Fail String deriving (Eq,Show)

run :: Test -> IO Result
run (Test _ reqOrRsp _ cs) = do
    dctx <- newContext 4096 -- FIXME
    ectx <- newContext 4096 -- FIXME
    let (dec,enc) = case reqOrRsp of
            "request" -> (decodeRequestHeader,  encodeRequestHeader)
            _         -> (decodeResponseHeader, encodeResponseHeader)
    testLoop cs dec dctx enc ectx

testLoop :: [Case]
         -> HPACKDecoding -> Context
         -> HPACKEncoding -> Context
         -> IO Result
testLoop [] _ _ _ _ = return Pass
testLoop (c:cs) dec dctx enc ectx = do
    res <- test c dec dctx enc ectx
    case res of
        Right (dctx', ectx') -> testLoop cs dec dctx' enc ectx'
        Left e               -> return $ Fail e

test :: Case
     -> HPACKDecoding -> Context
     -> HPACKEncoding -> Context
     -> IO (Either String (Context, Context))
test c dec dctx enc ectx = do
    x <- try $ dec inp dctx
    case x of
        Left (IndexOverrun idx) -> return $ Left $ "IndexOverrun " ++ show idx
        Right (hs',dctx') -> do
            (_, ectx') <- enc hs ectx
            let pass = sort hs == sort hs'
            if pass then
                return $ Right (dctx', ectx')
              else
                return $ Left $ "Headers are different in " ++ hex ++ ":\n" ++ show hs ++ "\n" ++ show hs'
  where
    hex = wire c
    inp = BS.pack $ fromHexString hex
    hs = headers c
