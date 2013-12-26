module HexString where

import Data.Word (Word8)
import Numeric
import Text.Printf

fromHexString :: String -> [Word8]
fromHexString = map fromHex . group2
  where
    fromHex = fst . head . readHex
    group2 [] = []
    group2 xs = ys : group2 zs
      where
       (ys,zs) = splitAt 2 xs

toHexString :: [Word8] -> String
toHexString = concatMap (printf "%02x")
