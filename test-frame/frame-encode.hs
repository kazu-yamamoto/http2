module Main where

import Control.Applicative ((<$>))
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.HashMap.Strict (union)
import Data.Hex
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)

import Case
import JSON
import Network.HTTP2.Decode
import Network.HTTP2.Encode
import Network.HTTP2.Types

main :: IO ()
main = undefined
