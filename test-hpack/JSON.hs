{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module JSON (
    Test (..),
    Case (..),
    Header,
) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as H
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector ((!))
import qualified Data.Vector as V
import Network.HPACK

{-
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    bs <- BL.getContents
    let Right x = eitherDecode bs :: Either String Test
    BL.putStr $ encodePretty x
-}

data Test = Test
    { description :: String
    , cases :: [Case]
    }
    deriving (Show)

data Case = Case
    { size :: Maybe Int
    , wire :: ByteString
    , headers :: [Header]
    , seqno :: Maybe Int
    }
    deriving (Show)

instance FromJSON Test where
    parseJSON (Object o) =
        Test
            <$> o .: "description"
            <*> o .: "cases"
    parseJSON _ = mzero

instance ToJSON Test where
    toJSON (Test desc cs) =
        object
            [ "description" .= desc
            , "cases" .= cs
            ]

instance FromJSON Case where
    parseJSON (Object o) =
        Case
            <$> o .:? "header_table_size"
            <*> (textToByteString <$> (o .: "wire"))
            <*> o .: "headers"
            <*> o .:? "seqno"
    parseJSON _ = mzero

instance ToJSON Case where
    toJSON (Case (Just siz) w hs no) =
        object
            [ "header_table_size" .= siz
            , "wire" .= byteStringToText w
            , "headers" .= hs
            , "seqno" .= no
            ]
    toJSON (Case Nothing w hs no) =
        object
            [ "wire" .= byteStringToText w
            , "headers" .= hs
            , "seqno" .= no
            ]

instance {-# OVERLAPPING #-} FromJSON [Header] where
    parseJSON (Array a) = mapM parseJSON $ V.toList a
    parseJSON _ = mzero

instance {-# OVERLAPPING #-} ToJSON [Header] where
    toJSON hs = toJSON $ map toJSON hs

instance {-# OVERLAPPING #-} FromJSON Header where
    parseJSON (Array a) = pure (mk $ toKey (a ! 0), toValue (a ! 1)) -- old
      where
        toKey = toValue
    parseJSON (Object o) = pure (mk $ textToByteString $ Key.toText k, toValue v) -- new
      where
        (k, v) = case H.toList o of
            [] -> error "parseJSON"
            x : _ -> x
    parseJSON _ = mzero

instance {-# OVERLAPPING #-} ToJSON Header where
    toJSON (k, v) =
        object [Key.fromText (byteStringToText $ foldedCase k) .= byteStringToText v]

textToByteString :: Text -> ByteString
textToByteString = B8.pack . T.unpack

byteStringToText :: ByteString -> Text
byteStringToText = T.pack . B8.unpack

toValue :: Value -> ByteString
toValue (String s) = textToByteString s
toValue _ = error "toValue"
