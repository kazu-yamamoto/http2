module Main where

import Test.DocTest

main :: IO ()
main = doctest [
    "-XOverloadedStrings"
  , "Network/HPACK.hs"
  ]
