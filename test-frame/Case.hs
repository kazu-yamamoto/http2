{-# LANGUAGE RecordWildCards, CPP #-}

module Case where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16

import JSON
import Network.HTTP2.Frame

data CaseSource = CaseSource {
    cs_description :: String
  , cs_encodeinfo :: EncodeInfo
  , cs_payload :: FramePayload
  } deriving (Show,Read)

data CaseWire = CaseWire {
    wire_description :: String
  , wire_hex :: ByteString
  , wire_padding :: Maybe Pad
  , wire_error :: Maybe [ErrorCodeId]
  } deriving (Show,Read)

sourceToWire :: CaseSource -> CaseWire
sourceToWire CaseSource{..} = CaseWire {
    wire_description = cs_description
  , wire_hex = wire
  , wire_padding = Pad <$> encodePadding cs_encodeinfo
  , wire_error = Nothing
  }
  where
    frame = encodeFrame cs_encodeinfo cs_payload
    wire = B16.encode frame

wireToCase :: CaseWire -> Case
wireToCase CaseWire { wire_error = Nothing, ..} = Case {
    description = wire_description
  , wire = wire_hex
  , frame = Just $ FramePad frm wire_padding
  , err = Nothing
  }
  where
    -- fromJust is unsafe
    frm = case decodeFrame defaultSettings $ fst $ B16.decode wire_hex of
        Left  e -> error $ show e
        Right r -> r
wireToCase CaseWire { wire_error = Just e, ..} = Case {
    description = wire_description
  , wire = wire_hex
  , frame = Nothing
  , err = Just $ fromErrorCodeId <$> e
  }
