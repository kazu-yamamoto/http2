{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Case where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Hex
import Data.Maybe (fromJust)

import JSON
import Network.HTTP2.Decode
import Network.HTTP2.Encode
import Network.HTTP2.Types

data CaseSource = CaseSource {
    cs_description :: String
  , cs_encodeinfo :: EncodeInfo
  , cs_payload :: FramePayload
  } deriving (Show,Read)

data CaseWire = CaseWire {
    wire_description :: String
  , wire_hex :: ByteString
  , wire_padding :: Pad
  , wire_error :: Maybe [ErrorCode]
  } deriving (Show,Read)

sourceToWire :: CaseSource -> CaseWire
sourceToWire CaseSource{..} = CaseWire {
    wire_description = cs_description
  , wire_hex = wire
  , wire_padding = Pad $ encodePadding cs_encodeinfo
  , wire_error = Nothing
  }
  where
    frame = encodeFrame cs_encodeinfo cs_payload
    wire = hex frame

wireToCase :: CaseWire -> Case
wireToCase CaseWire { wire_error = Nothing, ..} = Case {
    draft = 14
  , description = wire_description
  , wire = wire_hex
  , frame = Right frm
  , padding = wire_padding
  }
  where
    -- fromJust is unsafe
    frm = case decodeFrame defaultSettings $ fromJust $ unhex wire_hex of
        Left  e -> error $ show e
        Right r -> r
wireToCase CaseWire { wire_error = Just e, ..} = Case {
    draft = 14
  , description = wire_description
  , wire = wire_hex
  , frame = Left $ errorCodeToWord32 <$> e
  , padding = wire_padding
  }
