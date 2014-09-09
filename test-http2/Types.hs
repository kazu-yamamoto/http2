{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

import Control.Applicative ((<$>))
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.HashMap.Strict (union)
import Data.Word (Word32)
import Data.Text (Text)
import qualified Data.Text as T

import Network.HTTP2.Decode
import Network.HTTP2.Encode
import Network.HTTP2.Types

byteStringToText :: ByteString -> Text
byteStringToText = T.pack . B8.unpack

data Err = Err {
    code :: [Word32]
  , reason :: String
  }

data Pad = Pad (Maybe Padding)

data Case = Case {
    draft :: Int
  , description :: String
  , wire :: ByteString
  , frame :: Frame
  , padding :: Pad
  , err :: Maybe Err
  }

instance ToJSON Err where
    toJSON Err{..} = object ["code" .= code, "description" .= reason ]

instance ToJSON FramePayload where
    toJSON (DataFrame bs) = object ["data" .= byteStringToText bs]

instance ToJSON (Frame,Pad) where
    toJSON (Frame{..},pad) = object [
        "length" .= payloadLength frameHeader
      , "type" .= frameTypeToWord8 (framePayloadToFrameType framePayload)
      , "flags" .= flags frameHeader
      , "stream_identifier" .= fromStreamIdentifier (streamId frameHeader)
      , "frame_payload" .= (toJSON framePayload +++ toJSON pad)
      ]

instance ToJSON Pad where
    toJSON (Pad padding) = object [
        "padding_length" .= (BS.length <$> padding)
      , "padding" .= (byteStringToText <$> padding)
      ]

instance ToJSON Case where
    toJSON Case{..} = object [
        "drat" .= draft
      , "description" .= description
      , "wire" .= ("fixme" :: String)
      , "frame" .= (frame,padding)
      , "err" .= err
      ]

(+++) :: Value -> Value -> Value
Object x +++ Object y = Object $ x `union` y
_ +++ _ = error "+++"

testEncodeInfo = EncodeInfo {
    encodeFlags = defaultFlags
  , encodeStreamId = StreamIdentifier 2
  , encodePadding = Just "howdy!"
  }

testPayload = DataFrame "Hello, world!"

testFrame = frame
  where
    Right frame = decodeFrame defaultSettings $ encodeFrame testEncodeInfo testPayload

testCase = Case {
    draft = 14
  , description = "nomarl data frame"
  , wire = ""
  , frame = testFrame
  , err = Nothing
  , padding = Pad $ Just "howdy!"
  }