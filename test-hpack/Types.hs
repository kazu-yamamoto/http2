{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types (
    Test(..)
  , Case(..)
  , HeaderSet
  ) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Char8 as B8
import Data.CaseInsensitive (mk, foldedCase)
import qualified Data.Text as T
import Data.Vector ((!))
import qualified Data.Vector as V
import Network.HPACK
import Network.HTTP.Types

{-
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    bs <- BL.getContents
    let Right x = eitherDecode bs :: Either String Test
    BL.putStr $ encodePretty x
-}

data Test = Test {
    draft :: Int
  , context :: String
  , description :: String
  , cases :: [Case]
  } deriving Show

data Case = Case {
    size :: Int
  , wire :: String
  , headers :: HeaderSet
  } deriving Show

instance FromJSON Test where
    parseJSON (Object o) = Test <$> o .: "draft"
                                <*> o .: "context"
                                <*> o .: "description"
                                <*> o .: "cases"
    parseJSON _          = mzero

instance ToJSON Test where
    toJSON (Test n ctx desc cs) = object ["draft" .= n
                                         ,"context" .= ctx
                                         ,"description" .= desc
                                         ,"cases" .= cs
                                         ]

instance FromJSON Case where
    parseJSON (Object o) = Case <$> o .: "header_table_size"
                                <*> o .: "wire"
                                <*> o .: "headers"
    parseJSON _          = mzero

instance ToJSON Case where
    toJSON (Case siz w hs) = object ["header_table_size" .= siz
                                    ,"wire" .= w
                                    ,"headers" .= hs
                                    ]

instance FromJSON HeaderSet where
    parseJSON (Array a) = mapM parseJSON $ V.toList a
    parseJSON _         = mzero

instance ToJSON HeaderSet where
    toJSON hs = toJSON $ map toJSON hs

instance FromJSON Header where
    parseJSON (Array a) = pure (toKey (a ! 0), toValue (a ! 1))
      where
        toValue (String s) = B8.pack $ T.unpack s
        toValue _          = error "toValue"
        toKey = mk . toValue
    parseJSON _         = mzero

instance ToJSON Header where
--    toJSON (k,v) = object [ T.pack (B8.unpack (foldedCase k)) .= v]
    toJSON (k,v) = toJSON [foldedCase k,v]
