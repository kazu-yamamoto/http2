{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment (getArgs)

import Case

main :: IO ()
main = do
    args <- getArgs
    xs <- getContents
    if length args /= 0 then -- "-w"
        printWire xs
      else
        printJSON xs

printWire :: String -> IO ()
printWire = print . sourceToWire . read

printJSON :: String -> IO ()
printJSON = BL.putStrLn . encodePretty . toJSON . wireToCase . read
