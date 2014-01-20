module Main where

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import HPACK
import Types

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 2) $ do
        hPutStrLn stderr "hpack-encode on/off naive|linear|diff"
        exitFailure
    let [arg1,arg2] = args
        huffman
          | arg1 == "on" = True
          | otherwise = False
        algo
          | arg2 == "naive"  = Naive
          | arg2 == "linear" = Linear
          | otherwise        = Diff
        stgy = EncodeStrategy algo huffman
    hpackEncode stgy

hpackEncode :: EncodeStrategy -> IO ()
hpackEncode stgy = do
    bs <- BL.getContents
    let Just tc = decode bs :: Maybe Test
    Pass hexs <- run False stgy tc
    let cs = cases tc
        cs' = zipWith update cs hexs
        tc' = tc {
            description = "Encoded by the http2 library in Haskell. First clear the reference set and encode all headers"
          , cases = cs'
          }
    BL.putStrLn $ encodePretty tc'
  where
    update c hex = c { wire = hex }
