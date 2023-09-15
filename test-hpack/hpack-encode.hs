module Main where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import HPACKEncode
import JSON

main :: IO ()
main = do
    args <- getArgs
    (arg1,arg2,desc) <- case args of
      [a,b,c] -> return (a,b,c)
      _     -> do
        hPutStrLn stderr "hpack-encode on/off naive|linear <desc>"
        exitFailure
    let huffman
          | arg1 == "on" = True
          | otherwise = False
        algo
          | arg2 == "naive"  = Naive
          | arg2 == "static" = Static
          | otherwise        = Linear
        stgy = EncodeStrategy algo huffman
    hpackEncode stgy desc

hpackEncode :: EncodeStrategy -> String -> IO ()
hpackEncode stgy desc = do
    bs <- BL.getContents
    let tc = fromJust (decode bs :: Maybe Test)
    hexs <- run False stgy tc
    let cs = cases tc
        cs' = zipWith update cs hexs
        tc' = tc {
            description = desc
          , cases = cs'
          }
    BL.putStrLn $ encodePretty tc'
  where
    update c hex = c { wire = hex }
