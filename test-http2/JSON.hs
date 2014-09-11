{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module JSON where

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
import Data.Hex

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
      , "type" .= framePayloadToFrameTypeId framePayload
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

data CaseSource = CaseSource {
    cs_description :: String
  , cs_encodeinfo :: EncodeInfo
  , cs_payload :: FramePayload
  } deriving (Show,Read)

testSource :: CaseSource
testSource = CaseSource {
    cs_description = "noraml data frame"
  , cs_encodeinfo = EncodeInfo {
      encodeFlags = defaultFlags
    , encodeStreamId = StreamIdentifier 2
    , encodePadding = Just "howdy!"
    }
  , cs_payload = DataFrame "Hello, world!"
  }

data CaseWire = CaseWire {
    wire_description :: String
  , wire_hex :: ByteString
  } deriving (Show,Read)

sourceToWire :: CaseSource -> CaseWire
sourceToWire CaseSource{..} = CaseWire {
    wire_description = cs_description
  , wire_hex = wire
  }
  where
    frame = encodeFrame cs_encodeinfo cs_payload
    wire = hex frame
