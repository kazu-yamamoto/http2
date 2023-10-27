module Main where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL

import JSON

main :: IO ()
main = do
    bs <- BL.getContents
    let Just tc = decode bs :: Maybe Test
    let cs = cases tc
        cs' = zipWith update cs [0 ..]
        tc' = tc{cases = cs'}
    BL.putStrLn $ encodePretty tc'
  where
    update c i = c{seqno = Just i}
