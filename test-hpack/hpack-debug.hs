module Main where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL

import Types
import HPACKDecode

main :: IO ()
main = do
    bs <- BL.getContents
    let etc = eitherDecode bs :: Either String Test
    res <- case etc of
        Left e   -> return $ Just e
        Right tc -> do
            res <- run True tc
            case res of
                Pass   -> return Nothing
                Fail e -> return $ Just e
    print res
