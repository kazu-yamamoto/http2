{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module JSON where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Control.Monad (mzero)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.HashMap.Strict (union)
import Data.Text (Text)
import qualified Data.Text as T

import Network.HTTP2

----------------------------------------------------------------

byteStringToText :: ByteString -> Text
byteStringToText = T.pack . B8.unpack

(+++) :: Value -> Value -> Value
Object x +++ Object y = Object $ x `union` y
Null     +++ x        = x
x        +++ Null     = x
_        +++ _        = error "+++"

----------------------------------------------------------------

data FramePad = FramePad {
    fpFrame :: Frame
  , fpPad :: Maybe Pad
  } deriving (Show, Read)

data Pad = Pad Padding deriving (Show, Read)

----------------------------------------------------------------

data Case = Case {
    draft :: Int
  , description :: String
  , wire :: ByteString
  , frame :: Maybe FramePad
  , err :: Maybe [ErrorCode]
  } deriving (Show, Read)

----------------------------------------------------------------

instance ToJSON StreamIdentifier where
    toJSON (StreamIdentifier s) = toJSON s

instance FromJSON StreamIdentifier where
    parseJSON x = StreamIdentifier <$> parseJSON x

instance ToJSON Priority where
    toJSON (Priority e s w) = object [
        "exclusive" .= e
      , "stream_dependency" .= s
      , "weight" .= w
      ]

instance FromJSON Priority where
    parseJSON (Object o) = Priority <$> o .: "exclusive"
                                    <*> o .: "stream_dependency"
                                    <*> o .: "weight"
    parseJSON _          = mzero

instance ToJSON ErrorCodeId where
    toJSON e = toJSON $ fromErrorCodeId e

instance FromJSON ErrorCodeId where
    parseJSON e = toErrorCodeId <$> parseJSON e

----------------------------------------------------------------

instance ToJSON FramePayload where
    toJSON (DataFrame body) = object [
        "data" .= byteStringToText body
      ]
    toJSON (HeadersFrame mpri hdr) = object [
        "priority" .= mpri
      , "header_block_fragment" .= byteStringToText hdr
      ]
    toJSON (PriorityFrame pri) = object [
        "priority" .= pri
      ]
    toJSON (RSTStreamFrame e) = object [
        "error_code" .= e
      ]
    toJSON (SettingsFrame settings) = object [
        "settings" .= map (first fromSettingsKeyId) (fromSettings settings)
      ]
    toJSON (PushPromiseFrame sid hdr) = object [
        "promised_stream_id" .= sid
      , "header_block_fragment" .= byteStringToText hdr
      ]
    toJSON (PingFrame odata) = object [
        "opaque_data" .= byteStringToText odata
      ]
    toJSON (GoAwayFrame sid e debug) = object [
        "last_stream_id" .= sid
      , "error_code" .= e
      , "additional_debug_data" .= byteStringToText debug
      ]
    toJSON (WindowUpdateFrame size) = object [
        "window_size_increment" .= size
      ]
    toJSON (ContinuationFrame hdr) = object [
        "header_block_fragment" .= byteStringToText hdr
      ]
    toJSON (UnknownFrame _ opaque) = object [
        "payload" .= byteStringToText opaque
      ]

----------------------------------------------------------------

instance ToJSON FramePad where
    toJSON FramePad{fpFrame = Frame{..},..} = object [
        "length" .= payloadLength frameHeader
      , "type" .= framePayloadToFrameType framePayload
      , "flags" .= flags frameHeader
      , "stream_identifier" .= fromStreamIdentifier (streamId frameHeader)
      , "frame_payload" .= (toJSON framePayload +++ padObj)
      ]
      where
        padObj = case toJSON fpPad of
            Null -> emptyPad
            x    -> x

instance ToJSON Pad where
    toJSON (Pad padding) = object [
        "padding_length" .= BS.length padding
      , "padding" .= byteStringToText padding
      ]

emptyPad :: Value
emptyPad = object [
    "padding_length" .= Null
  , "padding" .= Null
  ]

----------------------------------------------------------------

instance ToJSON Case where
    toJSON Case{..} = object [
        "drat" .= draft
      , "description" .= description
      , "wire" .= byteStringToText wire
      , "frame" .= frame
      , "error" .= err
      ]
