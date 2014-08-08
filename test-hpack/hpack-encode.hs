module Main where

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import HPACKEncode
import Types

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 3) $ do
        hPutStrLn stderr "hpack-encode on/off naive|linear <desc>"
        exitFailure
    let [arg1,arg2,desc] = args
        huffman
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
    let Just tc = decode bs :: Maybe Test
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
