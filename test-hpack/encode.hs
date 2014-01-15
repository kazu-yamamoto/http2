module Main where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL

import HPACK
import Types

main :: IO ()
main = do
    bs <- BL.getContents
    let Just tc = decode bs :: Maybe Test
    Pass hexs <- run tc
    let cs = cases tc
        cs' = zipWith update cs hexs
        tc' = tc {
            description = "Encoded by the http2 library in Haskell. First clear the reference set and encode all headers"
          , cases = cs'
          }
    BL.putStrLn $ encodePretty tc'
  where
    update c hex = c { wire = hex }
