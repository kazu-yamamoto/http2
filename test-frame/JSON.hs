{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module JSON where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Arrow (first)
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.HashMap.Strict (union)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Network.HTTP2.Frame

----------------------------------------------------------------

byteStringToText :: ByteString -> Text
byteStringToText = T.pack . B8.unpack

textToByteString :: Text -> ByteString
textToByteString = B8.pack . T.unpack

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

unPad :: Pad -> Padding
unPad (Pad x) = x

----------------------------------------------------------------

data Case = Case {
    description :: String
  , wire :: ByteString
  , frame :: Maybe FramePad
  , err :: Maybe [ErrorCode]
  } deriving (Show, Read)

----------------------------------------------------------------

{-
instance ToJSON StreamIdentifier where
    toJSON (StreamIdentifier s) = toJSON s

instance FromJSON StreamIdentifier where
    parseJSON x = StreamIdentifier <$> parseJSON x
-}

instance ToJSON ErrorCodeId where
    toJSON e = toJSON $ fromErrorCodeId e

instance FromJSON ErrorCodeId where
    parseJSON e = toErrorCodeId <$> parseJSON e

instance {-# OVERLAPPING #-} ToJSON SettingsList where
    toJSON settings = toJSON $ map (first fromSettingsKeyId) settings

instance {-# OVERLAPPING #-} FromJSON SettingsList where
    parseJSON x = map (first (fromJust . toSettingsKeyId)) <$> parseJSON x

instance ToJSON ByteString where
    toJSON bs = toJSON $ byteStringToText bs

instance FromJSON ByteString where
    parseJSON x = textToByteString <$> parseJSON x

----------------------------------------------------------------

instance ToJSON FramePayload where
    toJSON (DataFrame body) = object [
        "data" .= body
      ]
    toJSON (HeadersFrame mpri hdr) = object [
        "exclusive" .= fromMaybe Null (toJSON . exclusive <$> mpri)
      , "stream_dependency" .= fromMaybe Null (toJSON . streamDependency <$> mpri)
      , "weight" .= fromMaybe Null (toJSON . weight <$> mpri)
      , "header_block_fragment" .= hdr
      ]
    toJSON (PriorityFrame pri) = object [
        "exclusive" .= exclusive pri
      , "stream_dependency" .= streamDependency pri
      , "weight" .= weight pri
      ]
    toJSON (RSTStreamFrame e) = object [
        "error_code" .= e
      ]
    toJSON (SettingsFrame settings) = object [
        "settings" .= settings
      ]
    toJSON (PushPromiseFrame sid hdr) = object [
        "promised_stream_id" .= sid
      , "header_block_fragment" .= hdr
      ]
    toJSON (PingFrame odata) = object [
        "opaque_data" .= odata
      ]
    toJSON (GoAwayFrame sid e debug) = object [
        "last_stream_id" .= sid
      , "error_code" .= e
      , "additional_debug_data" .= debug
      ]
    toJSON (WindowUpdateFrame size) = object [
        "window_size_increment" .= size
      ]
    toJSON (ContinuationFrame hdr) = object [
        "header_block_fragment" .= hdr
      ]
    toJSON (UnknownFrame _ opaque) = object [
        "payload" .= opaque
      ]

----------------------------------------------------------------

instance ToJSON FramePad where
    toJSON FramePad{fpFrame = Frame{..},..} = object [
        "length" .= payloadLength frameHeader
      , "type" .= fromFrameTypeId (framePayloadToFrameTypeId framePayload)
      , "flags" .= flags frameHeader
      , "stream_identifier" .= streamId frameHeader
      , "frame_payload" .= (toJSON framePayload +++ padObj)
      ]
      where
        padObj = case toJSON fpPad of
            Null
              | isPaddingDefined framePayload -> emptyPad
              | otherwise                     -> noPad
            x    -> x

instance FromJSON FramePad where
    parseJSON (Object o) = do
        len <- o .: "length"
        typ <- o .: "type"
        flg <- o .: "flags"
        sid <- o .: "stream_identifier"
        pld <- o .: "frame_payload"
        (payload,mpad) <- parsePayloadPad typ pld
        return FramePad {
            fpFrame = Frame {
                 frameHeader = FrameHeader len flg sid
               , framePayload = payload
               }
          , fpPad = mpad
          }
    parseJSON _ = mzero

parsePayloadPad :: FrameType -> Object -> Parser (FramePayload, Maybe Pad)
parsePayloadPad ftyp o = do
    mpad <- (Pad <$>) <$> o .:? "padding"
    payload <- parsePayload ftid o
    return (payload, mpad)
  where
    ftid = toFrameTypeId ftyp

priority :: Object -> Parser Priority
priority o = Priority <$> o .: "exclusive"
                       <*> o .: "stream_dependency"
                       <*> o .: "weight"

mpriority :: Object -> Parser (Maybe Priority)
mpriority o = do
    me <- o .:? "exclusive"
    ms <- o .:? "stream_dependency"
    mw <- o .:? "weight"
    return $ case me of
        Nothing -> Nothing
        Just ex -> Just $ Priority ex (fromJust ms) (fromJust mw)

parsePayload :: FrameTypeId -> Object -> Parser FramePayload
parsePayload FrameData o = DataFrame <$> o .: "data"
parsePayload FrameHeaders o = do
    mpri <- mpriority o
    hdr <- o .: "header_block_fragment"
    return $ HeadersFrame mpri hdr
parsePayload FramePriority o = PriorityFrame <$> priority o
parsePayload FrameRSTStream o = RSTStreamFrame <$> o .: "error_code"
parsePayload FrameSettings o = SettingsFrame <$> o .: "settings"
parsePayload FramePushPromise o = PushPromiseFrame <$> o .: "promised_stream_id"
                                                   <*> o .: "header_block_fragment"
parsePayload FramePing o = PingFrame <$> o .: "opaque_data"
parsePayload FrameGoAway o = GoAwayFrame <$> o .: "last_stream_id"
                                         <*> o .: "error_code"
                                         <*> o .: "additional_debug_data"
parsePayload FrameWindowUpdate o = WindowUpdateFrame <$> o .: "window_size_increment"
parsePayload FrameContinuation o = ContinuationFrame <$> o .: "header_block_fragment"
parsePayload (FrameUnknown typ) o = UnknownFrame typ <$> o .: "dummy"

instance ToJSON Pad where
    toJSON (Pad padding) = object [
        "padding_length" .= BS.length padding
      , "padding" .= padding
      ]

emptyPad :: Value
emptyPad = object [
    "padding_length" .= Null
  , "padding" .= Null
  ]

noPad :: Value
noPad = object []

----------------------------------------------------------------

instance ToJSON Case where
    toJSON Case{..} = object [
        "description" .= description
      , "wire" .= wire
      , "frame" .= frame
      , "error" .= err
      ]

instance FromJSON Case where
    parseJSON (Object o) = Case <$> o .: "description"
                                <*> o .: "wire"
                                <*> o .:? "frame"
                                <*> o .:? "error"
    parseJSON _          = mzero
