{-# LANGUAGE OverloadedStrings #-}

module Network.HPACK.Table.Token where

import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(..), memcmp)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.CaseInsensitive (mk, CI(..))

-- $setup
-- >>> :set -XOverloadedStrings

data Token = Token !Int !Bool !(CI ByteString) deriving (Eq, Show)

{-# INLINE toIx #-}
toIx :: Token -> Int
toIx (Token n _ _) = n

{-# INLINE shouldBeIndexed #-}
shouldBeIndexed :: Token -> Bool
shouldBeIndexed (Token _ b _) = b

tokenAuthority                :: Token
tokenMethod                   :: Token
tokenPath                     :: Token
tokenScheme                   :: Token
tokenStatus                   :: Token
tokenAcceptCharset            :: Token
tokenAcceptEncoding           :: Token
tokenAcceptLanguage           :: Token
tokenAcceptRanges             :: Token
tokenAccept                   :: Token
tokenAccessControlAllowOrigin :: Token
tokenAge                      :: Token
tokenAllow                    :: Token
tokenAuthorization            :: Token
tokenCacheControl             :: Token
tokenContentDisposition       :: Token
tokenContentEncoding          :: Token
tokenContentLanguage          :: Token
tokenContentLength            :: Token
tokenContentLocation          :: Token
tokenContentRange             :: Token
tokenContentType              :: Token
tokenCookie                   :: Token
tokenDate                     :: Token
tokenEtag                     :: Token
tokenExpect                   :: Token
tokenExpires                  :: Token
tokenFrom                     :: Token
tokenHost                     :: Token
tokenIfMatch                  :: Token
tokenIfModifiedSince          :: Token
tokenIfNoneMatch              :: Token
tokenIfRange                  :: Token
tokenIfUnmodifiedSince        :: Token
tokenLastModified             :: Token
tokenLink                     :: Token
tokenLocation                 :: Token
tokenMaxForwards              :: Token
tokenProxyAuthenticate        :: Token
tokenProxyAuthorization       :: Token
tokenRange                    :: Token
tokenReferer                  :: Token
tokenRefresh                  :: Token
tokenRetryAfter               :: Token
tokenServer                   :: Token
tokenSetCookie                :: Token
tokenStrictTransportSecurity  :: Token
tokenTransferEncoding         :: Token
tokenUserAgent                :: Token
tokenVary                     :: Token
tokenVia                      :: Token
tokenWwwAuthenticate          :: Token
tokenDummy                    :: Token

tokenAuthority                = Token  0  True ":authority"
tokenMethod                   = Token  1  True ":method"
tokenPath                     = Token  2 False ":path"
tokenScheme                   = Token  3  True ":scheme"
tokenStatus                   = Token  4  True ":status"
tokenAcceptCharset            = Token  5  True "accept-charset"
tokenAcceptEncoding           = Token  6  True "accept-encoding"
tokenAcceptLanguage           = Token  7  True "accept-language"
tokenAcceptRanges             = Token  8  True "accept-ranges"
tokenAccept                   = Token  9  True "accept"
tokenAccessControlAllowOrigin = Token 10  True "access-control-allow-origin"
tokenAge                      = Token 11  True "age"
tokenAllow                    = Token 12  True "allow"
tokenAuthorization            = Token 13  True "authorization"
tokenCacheControl             = Token 14  True "cache-control"
tokenContentDisposition       = Token 15  True "content-disposition"
tokenContentEncoding          = Token 16  True "content-encoding"
tokenContentLanguage          = Token 17  True "content-language"
tokenContentLength            = Token 18 False "content-length"
tokenContentLocation          = Token 19 False "content-location"
tokenContentRange             = Token 20  True "content-range"
tokenContentType              = Token 21  True "content-type"
tokenCookie                   = Token 22  True "cookie"
tokenDate                     = Token 23  True "date"
tokenEtag                     = Token 24 False "etag"
tokenExpect                   = Token 25  True "expect"
tokenExpires                  = Token 26  True "expires"
tokenFrom                     = Token 27  True "from"
tokenHost                     = Token 28  True "host"
tokenIfMatch                  = Token 29  True "if-match"
tokenIfModifiedSince          = Token 30  True "if-modified-since"
tokenIfNoneMatch              = Token 31  True "if-none-match"
tokenIfRange                  = Token 32  True "if-range"
tokenIfUnmodifiedSince        = Token 33  True "if-unmodified-since"
tokenLastModified             = Token 34  True "last-modified"
tokenLink                     = Token 35  True "link"
tokenLocation                 = Token 36  True "location"
tokenMaxForwards              = Token 37  True "max-forwards"
tokenProxyAuthenticate        = Token 38  True "proxy-authenticate"
tokenProxyAuthorization       = Token 39  True "proxy-authorization"
tokenRange                    = Token 40  True "range"
tokenReferer                  = Token 41  True "referer"
tokenRefresh                  = Token 42  True "refresh"
tokenRetryAfter               = Token 43  True "retry-after"
tokenServer                   = Token 44  True "server"
tokenSetCookie                = Token 45 False "set-cookie"
tokenStrictTransportSecurity  = Token 46  True "strict-transport-security"
tokenTransferEncoding         = Token 47  True "transfer-encoding"
tokenUserAgent                = Token 48  True "user-agent"
tokenVary                     = Token 49  True "vary"
tokenVia                      = Token 50  True "via"
tokenWwwAuthenticate          = Token 51  True "www-authenticate"
tokenDummy                    = Token 52  True "dummy"

minToken :: Int
minToken = 0

maxToken :: Int
maxToken = 51

otherToken :: Int
otherToken = maxToken + 1

{-# INLINE isTokenOther #-}
isTokenOther :: Token -> Bool
isTokenOther t = toIx t == otherToken

-- |
--
-- >>> toToken ":authority" == tokenAuthority
-- True
-- >>> toToken "foo"
-- Token 52 True "foo"
toToken :: ByteString -> Token
toToken bs = case len of
    3 -> case lst of
        97  -> if bs === "via" then tokenVia else mkTokenOther bs
        101 -> if bs === "age" then tokenAge else mkTokenOther bs
        _   -> mkTokenOther bs
    4 -> case lst of
        101 -> if bs === "date" then tokenDate else mkTokenOther bs
        103 -> if bs === "etag" then tokenEtag else mkTokenOther bs
        107 -> if bs === "link" then tokenLink else mkTokenOther bs
        109 -> if bs === "from" then tokenFrom else mkTokenOther bs
        116 -> if bs === "host" then tokenHost else mkTokenOther bs
        121 -> if bs === "vary" then tokenVary else mkTokenOther bs
        _   -> mkTokenOther bs
    5 -> case lst of
        101 -> if bs === "range" then tokenRange else mkTokenOther bs
        104 -> if bs === ":path" then tokenPath else mkTokenOther bs
        119 -> if bs === "allow" then tokenAllow else mkTokenOther bs
        _   -> mkTokenOther bs
    6 -> case lst of
        101 -> if bs === "cookie" then tokenCookie else mkTokenOther bs
        114 -> if bs === "server" then tokenServer else mkTokenOther bs
        116 -> if bs === "expect" then tokenExpect else
               if bs === "accept" then tokenAccept else mkTokenOther bs
        _   -> mkTokenOther bs
    7 -> case lst of
        100 -> if bs === ":method" then tokenMethod else mkTokenOther bs
        101 -> if bs === ":scheme" then tokenScheme else mkTokenOther bs
        104 -> if bs === "refresh" then tokenRefresh else mkTokenOther bs
        114 -> if bs === "referer" then tokenReferer else mkTokenOther bs
        115 -> if bs === "expires" then tokenExpires else
               if bs === ":status" then tokenStatus else mkTokenOther bs
        _   -> mkTokenOther bs
    8 -> case lst of
        101 -> if bs === "if-range" then tokenIfRange else mkTokenOther bs
        104 -> if bs === "if-match" then tokenIfMatch else mkTokenOther bs
        110 -> if bs === "location" then tokenLocation else mkTokenOther bs
        _   -> mkTokenOther bs
    10 -> case lst of
        101 -> if bs === "set-cookie" then tokenSetCookie else mkTokenOther bs
        116 -> if bs === "user-agent" then tokenUserAgent else mkTokenOther bs
        121 -> if bs === ":authority" then tokenAuthority else mkTokenOther bs
        _   -> mkTokenOther bs
    11 -> case lst of
        114 -> if bs === "retry-after" then tokenRetryAfter else mkTokenOther bs
        _   -> mkTokenOther bs
    12 -> case lst of
        101 -> if bs === "content-type" then tokenContentType else mkTokenOther bs
        115 -> if bs === "max-forwards" then tokenMaxForwards else mkTokenOther bs
        _   -> mkTokenOther bs
    13 -> case lst of
        100 -> if bs === "last-modified" then tokenLastModified else mkTokenOther bs
        101 -> if bs === "content-range" then tokenContentRange else mkTokenOther bs
        104 -> if bs === "if-none-match" then tokenIfNoneMatch else mkTokenOther bs
        108 -> if bs === "cache-control" then tokenCacheControl else mkTokenOther bs
        110 -> if bs === "authorization" then tokenAuthorization else mkTokenOther bs
        115 -> if bs === "accept-ranges" then tokenAcceptRanges else mkTokenOther bs
        _   -> mkTokenOther bs
    14 -> case lst of
        104 -> if bs === "content-length" then tokenContentLength else mkTokenOther bs
        116 -> if bs === "accept-charset" then tokenAcceptCharset else mkTokenOther bs
        _   -> mkTokenOther bs
    15 -> case lst of
        101 -> if bs === "accept-language" then tokenAcceptLanguage else mkTokenOther bs
        103 -> if bs === "accept-encoding" then tokenAcceptEncoding else mkTokenOther bs
        _   -> mkTokenOther bs
    16 -> case lst of
        101 -> if bs === "content-language" then tokenContentLanguage else
               if bs === "www-authenticate" then tokenWwwAuthenticate else mkTokenOther bs
        103 -> if bs === "content-encoding" then tokenContentEncoding else mkTokenOther bs
        110 -> if bs === "content-location" then tokenContentLocation else mkTokenOther bs
        _   -> mkTokenOther bs
    17 -> case lst of
        101 -> if bs === "if-modified-since" then tokenIfModifiedSince else mkTokenOther bs
        103 -> if bs === "transfer-encoding" then tokenTransferEncoding else mkTokenOther bs
        _   -> mkTokenOther bs
    18 -> case lst of
        101 -> if bs === "proxy-authenticate" then tokenProxyAuthenticate else mkTokenOther bs
        _   -> mkTokenOther bs
    19 -> case lst of
        101 -> if bs === "if-unmodified-since" then tokenIfUnmodifiedSince else mkTokenOther bs
        110 -> if bs === "proxy-authorization" then tokenProxyAuthorization else
               if bs === "content-disposition" then tokenContentDisposition else mkTokenOther bs
        _   -> mkTokenOther bs
    25 -> case lst of
        121 -> if bs === "strict-transport-security" then tokenStrictTransportSecurity else mkTokenOther bs
        _   -> mkTokenOther bs
    27 -> case lst of
        110 -> if bs === "access-control-allow-origin" then tokenAccessControlAllowOrigin else mkTokenOther bs
        _   -> mkTokenOther bs
    _ -> mkTokenOther bs
  where
    len = B.length bs
    lst = B.last bs
    PS fp1 off1 siz === PS fp2 off2 _ = unsafeDupablePerformIO $
      withForeignPtr fp1 $ \p1 ->
      withForeignPtr fp2 $ \p2 -> do
        i <- memcmp (p1 `plusPtr` off1) (p2 `plusPtr` off2) siz
        return $! i == 0

mkTokenOther :: ByteString -> Token
mkTokenOther bs = Token otherToken True (mk bs)
