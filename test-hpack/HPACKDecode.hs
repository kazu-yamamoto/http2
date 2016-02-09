{-# LANGUAGE OverloadedStrings, CPP #-}

module HPACKDecode (
    run
  , Result(..)
  , EncodeStrategy(..)
  , defaultEncodeStrategy
  , CompressionAlgo(..)
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Exception
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B8
import Data.Hex
import Data.List (sort)
import Network.HPACK
import Network.HPACK.Table

import JSON

data Conf = Conf {
    debug :: Bool
  }

data Result = Pass | Fail String deriving (Eq,Show)

run :: Bool -> Test -> IO Result
run _ (Test _ [])        = return $ Pass
run d (Test _ ccs) = do
    -- 'size c' must not be used. Initial value is 4,096!
    dyntbl <- newDynamicTableForDecoding 4096
    let conf = Conf { debug = d }
    testLoop conf ccs dyntbl

testLoop :: Conf
         -> [Case]
         -> DynamicTable
         -> IO Result
testLoop _    []     _      = return $ Pass
testLoop conf (c:cs) dyntbl = do
    res <- test conf c dyntbl
    case res of
        Nothing -> testLoop conf cs dyntbl
        Just  e -> return $ Fail e

test :: Conf
     -> Case
     -> DynamicTable
     -> IO (Maybe String)
test conf c dyntbl = do
    -- context is destructive!!!
    when (debug conf) $ do
        putStrLn "--------------------------------"
        putStrLn "---- Input header list"
        printHeaderList $ sort hs
        putStrLn "---- Input header table"
        printDynamicTable dyntbl
        putStrLn "---- Input Hex"
        B8.putStrLn wirehex
    case size c of
        Nothing  -> return ()
        Just siz -> renewDynamicTable siz dyntbl
    x <- try $ decodeHeader dyntbl inp
    case x of
        Left e -> return $ Just $ show (e :: DecodeError)
        Right hs' -> do
            let pass = sort hs == sort hs'
            if pass then
                return Nothing
              else
                return $ Just $ "Headers are different in " ++ B8.unpack wirehex ++ ":\n" ++ show hs ++ "\n" ++ show hs'
  where
    wirehex = wire c
    Just inp = unhex wirehex
    hs = headers c

-- | Printing 'HeaderList'.
printHeaderList :: HeaderList -> IO ()
printHeaderList hs = mapM_ printHeader hs
  where
    printHeader (k,v) = do
        B8.putStr k
        putStr ": "
        B8.putStr v
        putStr "\n"
