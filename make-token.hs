{-# LANGUAGE OverloadedStrings #-}

-- Need to fix "Pattern match(es) are overlapped" by hand

module Main where

import Control.Arrow
import Data.Char
import Data.List

main :: IO ()
main = do
    putStrLn "{-# LANGUAGE OverloadedStrings #-}"
    putStr "\n"
    putStrLn "module Token where"
    putStr "\n"
    putStrLn "import Data.ByteString (ByteString)"
    putStrLn "import qualified Data.ByteString as B"
    putStrLn "import Data.Ix"
    putStr "\n"
    putStrLn "-- $setup"
    putStrLn "-- >>> :set -XOverloadedStrings"
    putStr "\n"
    printTokens tokens
    putStr "\n"
    printDoctests targets
    printToToken targets
  where
    uniqKeys = map head $ group $ map fst staticTableList
    tokens = map mkToken uniqKeys
    targets = zip uniqKeys tokens

mkToken :: String -> String
mkToken key = "T" ++ concatMap capitalize (splitBy isAlphaNum key)

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs

splitBy :: (Char -> Bool) -> String -> [String]
splitBy _ [] = []
splitBy p xs
    | r == ""   = splitBy p xs''
    | otherwise = r : splitBy p xs''
  where
    (r,xs') = span p xs
    xs'' = dropWhile (not . p) xs'

printDoctests :: [(String,String)] -> IO ()
printDoctests xs = do
    putStrLn "-- |"
    putStrLn "--"
    mapM_ pr xs
    pr ("foo",tokenOther)
  where
    pr (k,t) = do
        putStrLn $ "-- >>> toToken \"" ++ k ++ "\""
        putStrLn $ "-- " ++ t

tokenOther :: String
tokenOther = "TOTHER"

printTokens :: [String] -> IO ()
printTokens []     = return ()
printTokens (t:ts) = do
    putStrLn $ "data Token = " ++ t
    mapM_ pr ts
    putStrLn $ "           | " ++ tokenOther
    putStrLn   "           deriving (Eq,Ord,Show,Enum,Bounded,Ix)"
  where
    pr x = putStrLn $ "           | " ++ x

printToToken :: [(String,String)] -> IO ()
printToToken xs = do
    putStrLn "toToken :: ByteString -> Token"
    putStrLn "toToken bs = case len of"
    mapM_ printCase ys
    putStrLn $ "    _ -> " ++ tokenOther
    putStrLn "  where"
    putStrLn "    len = B.length bs"
    putStrLn "    lst = B.last bs"
  where
    addLen kv@(k,_) = (length k, kv)
    extract zs = (l,kv)
      where
        l = fst $ head zs
        (_,kv) = unzip zs
    ys = map extract $ groupBy (\x y -> fst x == fst y) $ sort $ map addLen xs

printCase :: (Int, [(String,String)]) -> IO ()
printCase (l,xs) = do
    putStrLn $ "    " ++ show l ++ " -> case lst of"
    mapM_ pr xs'
    putStrLn $ "        _   -> " ++ tokenOther
  where
    xs' = map (first reverse) $ sort $ map (first reverse) xs
    pr (k,t) = do
        putStrLn $ "        " ++ show w ++ " -> if bs == \"" ++ k ++ "\" then " ++ t ++ " else " ++ tokenOther
      where
        w = ord $ last k

staticTableList :: [(String, String)]
staticTableList = [
    (":authority", "")
  , (":method", "GET")
  , (":method", "POST")
  , (":path", "/")
  , (":path", "/index.html")
  , (":scheme", "http")
  , (":scheme", "https")
  , (":status", "200")
  , (":status", "204")
  , (":status", "206")
  , (":status", "304")
  , (":status", "400")
  , (":status", "404")
  , (":status", "500")
  , ("accept-charset", "")
  , ("accept-encoding", "gzip, deflate")
  , ("accept-language", "")
  , ("accept-ranges", "")
  , ("accept", "")
  , ("access-control-allow-origin", "")
  , ("age", "")
  , ("allow", "")
  , ("authorization", "")
  , ("cache-control", "")
  , ("content-disposition", "")
  , ("content-encoding", "")
  , ("content-language", "")
  , ("content-length", "")
  , ("content-location", "")
  , ("content-range", "")
  , ("content-type", "")
  , ("cookie", "")
  , ("date", "")
  , ("etag", "")
  , ("expect", "")
  , ("expires", "")
  , ("from", "")
  , ("host", "")
  , ("if-match", "")
  , ("if-modified-since", "")
  , ("if-none-match", "")
  , ("if-range", "")
  , ("if-unmodified-since", "")
  , ("last-modified", "")
  , ("link", "")
  , ("location", "")
  , ("max-forwards", "")
  , ("proxy-authenticate", "")
  , ("proxy-authorization", "")
  , ("range", "")
  , ("referer", "")
  , ("refresh", "")
  , ("retry-after", "")
  , ("server", "")
  , ("set-cookie", "")
  , ("strict-transport-security", "")
  , ("transfer-encoding", "")
  , ("user-agent", "")
  , ("vary", "")
  , ("via", "")
  , ("www-authenticate", "")
  ]
