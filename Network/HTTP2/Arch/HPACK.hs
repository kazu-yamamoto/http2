{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Arch.HPACK (
    hpackEncodeHeader
  , hpackEncodeHeaderLoop
  , hpackDecodeHeader
  , hpackDecodeTrailer
  , just
  , fixHeaders
  ) where

import qualified Control.Exception as E
import Network.ByteOrder
import qualified Network.HTTP.Types as H

import Imports
import Network.HPACK
import Network.HPACK.Token
import Network.HTTP2.Arch.Context
import Network.HTTP2.Frame

-- $setup
-- >>> :set -XOverloadedStrings

----------------------------------------------------------------

fixHeaders :: H.ResponseHeaders -> H.ResponseHeaders
fixHeaders hdr = deleteUnnecessaryHeaders hdr

deleteUnnecessaryHeaders :: H.ResponseHeaders -> H.ResponseHeaders
deleteUnnecessaryHeaders hdr = filter del hdr
  where
    del (k,_) = k `notElem` headersToBeRemoved

headersToBeRemoved :: [H.HeaderName]
headersToBeRemoved = [ H.hConnection
                     , "Transfer-Encoding"
                     -- Keep-Alive
                     -- Proxy-Connection
                     -- Upgrade
                     ]

----------------------------------------------------------------

strategy :: EncodeStrategy
strategy = EncodeStrategy { compressionAlgo = Linear, useHuffman = False }

-- Set-Cookie: contains only one cookie value.
-- So, we don't need to split it.
hpackEncodeHeader :: Context -> Buffer -> BufferSize
                  -> TokenHeaderList
                  -> IO (TokenHeaderList, Int)
hpackEncodeHeader Context{..} buf siz ths =
    encodeTokenHeader buf siz strategy True encodeDynamicTable ths

hpackEncodeHeaderLoop :: Context -> Buffer -> BufferSize
                      -> TokenHeaderList
                      -> IO (TokenHeaderList, Int)
hpackEncodeHeaderLoop Context{..} buf siz hs =
    encodeTokenHeader buf siz strategy False encodeDynamicTable hs

----------------------------------------------------------------

hpackDecodeHeader :: HeaderBlockFragment -> Context -> IO HeaderTable
hpackDecodeHeader hdrblk ctx = do
    tbl@(_,vt) <- hpackDecodeTrailer hdrblk ctx
    if isClient ctx then
        return tbl
    else if checkRequestHeader vt then
        return tbl
      else
        E.throwIO $ ConnectionError ProtocolError "the header key is illegal"

hpackDecodeTrailer :: HeaderBlockFragment -> Context -> IO HeaderTable
hpackDecodeTrailer hdrblk Context{..} = decodeTokenHeader decodeDynamicTable hdrblk `E.catch` handl
  where
    handl IllegalHeaderName =
        E.throwIO $ ConnectionError ProtocolError "the header key is illegal"
    handl _ =
        E.throwIO $ ConnectionError CompressionError "cannot decompress the header"

{-# INLINE checkRequestHeader #-}
checkRequestHeader :: ValueTable -> Bool
checkRequestHeader reqvt
  | just mMethod (== "CONNECT") = isNothing mPath && isNothing mScheme
  | isJust mStatus              = False
  | isNothing mMethod           = False
  | isNothing mScheme           = False
  | isNothing mPath             = False
  | mPath       == Just ""      = False
  | isJust mConnection          = False
  | just mTE (/= "trailers")    = False
  | otherwise                   = True
  where
    mStatus     = getHeaderValue tokenStatus reqvt
    mScheme     = getHeaderValue tokenScheme reqvt
    mPath       = getHeaderValue tokenPath reqvt
    mMethod     = getHeaderValue tokenMethod reqvt
    mConnection = getHeaderValue tokenConnection reqvt
    mTE         = getHeaderValue tokenTE reqvt

{-# INLINE just #-}
just :: Maybe a -> (a -> Bool) -> Bool
just Nothing  _    = False
just (Just x) p
  | p x            = True
  | otherwise      = False
