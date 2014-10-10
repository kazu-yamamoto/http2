{-# LANGUAGE OverloadedStrings #-}

module HPACKDecode (
    run
  , Result(..)
  , EncodeStrategy(..)
  , defaultEncodeStrategy
  , CompressionAlgo(..)
  ) where

import Control.Applicative ((<$>))
import Control.Exception
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B8
import Data.Hex
import Data.List (sort)
import Network.HPACK
import Network.HPACK.HeaderBlock
import Network.HPACK.Table

import JSON

data Conf = Conf {
    debug :: Bool
  }

data Result = Pass | Fail String deriving (Eq,Show)

run :: Bool -> Test -> IO Result
run _ (Test _ _ [])        = return $ Pass
run d (Test _ _ ccs@(c:_)) = do
    let siz = maybe 4096 id $ size c
    dhdrtbl <- newHeaderTableForDecoding siz
    let conf = Conf { debug = d }
    testLoop conf ccs dhdrtbl

testLoop :: Conf
         -> [Case]
         -> HeaderTable
         -> IO Result
testLoop _    []     _    = return $ Pass
testLoop conf (c:cs) dhdrtbl  = do
    res <- test conf c dhdrtbl
    case res of
        Right dhdrtbl' -> testLoop conf cs dhdrtbl'
        Left  e     -> return $ Fail e

test :: Conf
     -> Case
     -> HeaderTable
     -> IO (Either String HeaderTable)
test conf c dhdrtbl = do
    -- context is destructive!!!
    when (debug conf) $ do
        putStrLn "--------------------------------"
        putStrLn "---- Input header list"
        printHeaderList $ sort hs
        putStrLn "---- Input header table"
        printHeaderTable dhdrtbl
        putStrLn "---- Input Hex"
        B8.putStrLn wirehex
        putStrLn "---- Input header block"
        print bshd'
    dhdrtbl0 <- case size c of
        Nothing  -> return dhdrtbl
        Just siz
          | shouldRenew dhdrtbl siz -> renewHeaderTable siz dhdrtbl
          | otherwise               -> return dhdrtbl
    x <- try $ decodeHeader dhdrtbl0 inp
    case x of
        Left e -> return $ Left $ show (e :: DecodeError)
        Right (dhdrtbl',hs') -> do
            let pass = sort hs == sort hs'
            if pass then
                return $ Right (dhdrtbl')
              else
                return $ Left $ "Headers are different in " ++ B8.unpack wirehex ++ ":\n" ++ show hd ++ "\n" ++ show hs ++ "\n" ++ show hs'
  where
    wirehex = wire c
    Just inp = unhex wirehex
    hs = headers c
    bshd = fromByteStreamDebug inp
    hd = map snd <$> bshd
    bshd' = map (\(x,y)->(hex x,y)) <$> bshd

-- | Printing 'HeaderList'.
printHeaderList :: HeaderList -> IO ()
printHeaderList hs = mapM_ printHeader hs
  where
    printHeader (k,v) = do
        B8.putStr k
        putStr ": "
        B8.putStr v
        putStr "\n"
