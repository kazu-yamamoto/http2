module Main where

import Test.DocTest

main :: IO ()
main = do
    doctest [
        "-XOverloadedStrings"
      , "Network/HPACK.hs"
      ]
    doctest [
        "-XOverloadedStrings"
      , "Network/HTTP2.hs"
      ]
