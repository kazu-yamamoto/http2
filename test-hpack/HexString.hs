module HexString where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Numeric
import Text.Printf

fromHexString :: String -> ByteString
fromHexString = BS.pack . map fromHex . group2
  where
    fromHex = fst . head . readHex
    group2 [] = []
    group2 xs = ys : group2 zs
      where
       (ys,zs) = splitAt 2 xs

toHexString :: ByteString -> String
toHexString = concatMap (printf "%02x") . BS.unpack
