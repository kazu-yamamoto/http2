{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module JSON where

import Control.Applicative ((<$>))
import Data.Aeson
import Data.Array (assocs)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.HashMap.Strict (union)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)

import Network.HTTP2

----------------------------------------------------------------

byteStringToText :: ByteString -> Text
byteStringToText = T.pack . B8.unpack

(+++) :: Value -> Value -> Value
Object x +++ Object y = Object $ x `union` y
_ +++ _ = error "+++"

----------------------------------------------------------------

type EFrame = Either [Word32] Frame
data Pad = Pad (Maybe Padding) deriving (Show, Read)

----------------------------------------------------------------

data Case = Case {
    draft :: Int
  , description :: String
  , wire :: ByteString
  , frame :: EFrame
  , padding :: Pad
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
        "settings" .= alist3
      ]
      where
        alist1 = assocs settings
        alist2 = filter (isJust.snd) alist1
        alist3 = (\(x,y) -> (settingsToWord16 x, fromJust y)) <$> alist2
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

instance ToJSON (EFrame,Pad) where
    toJSON (Right Frame{..},pad) = object [
        "length" .= payloadLength frameHeader
      , "type" .= framePayloadToFrameTypeId framePayload
      , "flags" .= flags frameHeader
      , "stream_identifier" .= fromStreamIdentifier (streamId frameHeader)
      , "frame_payload" .= (toJSON framePayload +++ toJSON pad)
      ]
    toJSON (Left _, _) = Null

instance ToJSON Pad where
    toJSON (Pad padding) = object [
        "padding_length" .= (BS.length <$> padding)
      , "padding" .= (byteStringToText <$> padding)
      ]

instance ToJSON EFrame where
    toJSON (Right _) = Null
    toJSON (Left e) = toJSON e

----------------------------------------------------------------

instance ToJSON Case where
    toJSON Case{..} = object [
        "drat" .= draft
      , "description" .= description
      , "wire" .= byteStringToText wire
      , "frame" .= (frame,padding)
      , "err" .= frame
      ]
