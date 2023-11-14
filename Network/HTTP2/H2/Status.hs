{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2.H2.Status (
    getStatus,
    setStatus,
) where

import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Internal (unsafeCreate)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (poke)
import qualified Network.HTTP.Types as H

import Imports
import Network.HPACK
import Network.HPACK.Token

----------------------------------------------------------------

getStatus :: HeaderTable -> Maybe H.Status
getStatus (_, vt) = getHeaderValue tokenStatus vt >>= toStatus

setStatus :: H.Status -> H.ResponseHeaders -> H.ResponseHeaders
setStatus st hdr = (":status", fromStatus st) : hdr

----------------------------------------------------------------

fromStatus :: H.Status -> ByteString
fromStatus status = unsafeCreate 3 $ \p -> do
    poke p (toW8 r2)
    poke (p `plusPtr` 1) (toW8 r1)
    poke (p `plusPtr` 2) (toW8 r0)
  where
    toW8 :: Int -> Word8
    toW8 n = 48 + fromIntegral n
    s = H.statusCode status
    (q0, r0) = s `divMod` 10
    (q1, r1) = q0 `divMod` 10
    r2 = q1 `mod` 10

toStatus :: ByteString -> Maybe H.Status
toStatus bs = case C8.readInt bs of
    Nothing -> Nothing
    Just (code, _) -> Just $ toEnum code
