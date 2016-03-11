{-# LANGUAGE OverloadedStrings #-}

module Network.HPACK.Table.Token where

import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(..), memcmp)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- $setup
-- >>> :set -XOverloadedStrings

data Token = Token !Int deriving (Eq, Show)

fromToken :: Token -> Int
fromToken (Token n) = n

tokenAuthority                = Token 0
tokenMethod                   = Token 1
tokenPath                     = Token 2
tokenScheme                   = Token 3
tokenStatus                   = Token 4
tokenAcceptCharset            = Token 5
tokenAcceptEncoding           = Token 6
tokenAcceptLanguage           = Token 7
tokenAcceptRanges             = Token 8
tokenAccept                   = Token 9
tokenAccessControlAllowOrigin = Token 10
tokenAge                      = Token 11
tokenAllow                    = Token 12
tokenAuthorization            = Token 13
tokenCacheControl             = Token 14
tokenContentDisposition       = Token 15
tokenContentEncoding          = Token 16
tokenContentLanguage          = Token 17
tokenContentLength            = Token 18
tokenContentLocation          = Token 19
tokenContentRange             = Token 20
tokenContentType              = Token 21
tokenCookie                   = Token 22
tokenDate                     = Token 23
tokenEtag                     = Token 24
tokenExpect                   = Token 25
tokenExpires                  = Token 26
tokenFrom                     = Token 27
tokenHost                     = Token 28
tokenIfMatch                  = Token 29
tokenIfModifiedSince          = Token 30
tokenIfNoneMatch              = Token 31
tokenIfRange                  = Token 32
tokenIfUnmodifiedSince        = Token 33
tokenLastModified             = Token 34
tokenLink                     = Token 35
tokenLocation                 = Token 36
tokenMaxForwards              = Token 37
tokenProxyAuthenticate        = Token 38
tokenProxyAuthorization       = Token 39
tokenRange                    = Token 40
tokenReferer                  = Token 41
tokenRefresh                  = Token 42
tokenRetryAfter               = Token 43
tokenServer                   = Token 44
tokenSetCookie                = Token 45
tokenStrictTransportSecurity  = Token 46
tokenTransferEncoding         = Token 47
tokenUserAgent                = Token 48
tokenVary                     = Token 49
tokenVia                      = Token 50
tokenWwwAuthenticate          = Token 51
tokenOther                    = Token 52

-- |
--
-- >>> toToken ":authority"
-- Token 0
-- >>> toToken ":method"
-- Token 1
-- >>> toToken ":path"
-- Token 2
-- >>> toToken ":scheme"
-- Token 3
-- >>> toToken ":status"
-- Token 4
-- >>> toToken "accept-charset"
-- Token 5
-- >>> toToken "accept-encoding"
-- Token 6
-- >>> toToken "accept-language"
-- Token 7
-- >>> toToken "accept-ranges"
-- Token 8
-- >>> toToken "accept"
-- Token 9
-- >>> toToken "access-control-allow-origin"
-- Token 10
-- >>> toToken "age"
-- Token 11
-- >>> toToken "allow"
-- Token 12
-- >>> toToken "authorization"
-- Token 13
-- >>> toToken "cache-control"
-- Token 14
-- >>> toToken "content-disposition"
-- Token 15
-- >>> toToken "content-encoding"
-- Token 16
-- >>> toToken "content-language"
-- Token 17
-- >>> toToken "content-length"
-- Token 18
-- >>> toToken "content-location"
-- Token 19
-- >>> toToken "content-range"
-- Token 20
-- >>> toToken "content-type"
-- Token 21
-- >>> toToken "cookie"
-- Token 22
-- >>> toToken "date"
-- Token 23
-- >>> toToken "etag"
-- Token 24
-- >>> toToken "expect"
-- Token 25
-- >>> toToken "expires"
-- Token 26
-- >>> toToken "from"
-- Token 27
-- >>> toToken "host"
-- Token 28
-- >>> toToken "if-match"
-- Token 29
-- >>> toToken "if-modified-since"
-- Token 30
-- >>> toToken "if-none-match"
-- Token 31
-- >>> toToken "if-range"
-- Token 32
-- >>> toToken "if-unmodified-since"
-- Token 33
-- >>> toToken "last-modified"
-- Token 34
-- >>> toToken "link"
-- Token 35
-- >>> toToken "location"
-- Token 36
-- >>> toToken "max-forwards"
-- Token 37
-- >>> toToken "proxy-authenticate"
-- Token 38
-- >>> toToken "proxy-authorization"
-- Token 39
-- >>> toToken "range"
-- Token 40
-- >>> toToken "referer"
-- Token 41
-- >>> toToken "refresh"
-- Token 42
-- >>> toToken "retry-after"
-- Token 43
-- >>> toToken "server"
-- Token 44
-- >>> toToken "set-cookie"
-- Token 45
-- >>> toToken "strict-transport-security"
-- Token 46
-- >>> toToken "transfer-encoding"
-- Token 47
-- >>> toToken "user-agent"
-- Token 48
-- >>> toToken "vary"
-- Token 49
-- >>> toToken "via"
-- Token 50
-- >>> toToken "www-authenticate"
-- Token 51
-- >>> toToken "foo"
-- Token 52
toToken :: ByteString -> Token
toToken bs = case len of
    3 -> case lst of
        97  -> if bs === "via" then tokenVia else tokenOther
        101 -> if bs === "age" then tokenAge else tokenOther
        _   -> tokenOther
    4 -> case lst of
        101 -> if bs === "date" then tokenDate else tokenOther
        103 -> if bs === "etag" then tokenEtag else tokenOther
        107 -> if bs === "link" then tokenLink else tokenOther
        109 -> if bs === "from" then tokenFrom else tokenOther
        116 -> if bs === "host" then tokenHost else tokenOther
        121 -> if bs === "vary" then tokenVary else tokenOther
        _   -> tokenOther
    5 -> case lst of
        101 -> if bs === "range" then tokenRange else tokenOther
        104 -> if bs === ":path" then tokenPath else tokenOther
        119 -> if bs === "allow" then tokenAllow else tokenOther
        _   -> tokenOther
    6 -> case lst of
        101 -> if bs === "cookie" then tokenCookie else tokenOther
        114 -> if bs === "server" then tokenServer else tokenOther
        116 -> if bs === "expect" then tokenExpect else
               if bs === "accept" then tokenAccept else tokenOther
        _   -> tokenOther
    7 -> case lst of
        100 -> if bs === ":method" then tokenMethod else tokenOther
        101 -> if bs === ":scheme" then tokenScheme else tokenOther
        104 -> if bs === "refresh" then tokenRefresh else tokenOther
        114 -> if bs === "referer" then tokenReferer else tokenOther
        115 -> if bs === "expires" then tokenExpires else
               if bs === ":status" then tokenStatus else tokenOther
        _   -> tokenOther
    8 -> case lst of
        101 -> if bs === "if-range" then tokenIfRange else tokenOther
        104 -> if bs === "if-match" then tokenIfMatch else tokenOther
        110 -> if bs === "location" then tokenLocation else tokenOther
        _   -> tokenOther
    10 -> case lst of
        101 -> if bs === "set-cookie" then tokenSetCookie else tokenOther
        116 -> if bs === "user-agent" then tokenUserAgent else tokenOther
        121 -> if bs === ":authority" then tokenAuthority else tokenOther
        _   -> tokenOther
    11 -> case lst of
        114 -> if bs === "retry-after" then tokenRetryAfter else tokenOther
        _   -> tokenOther
    12 -> case lst of
        101 -> if bs === "content-type" then tokenContentType else tokenOther
        115 -> if bs === "max-forwards" then tokenMaxForwards else tokenOther
        _   -> tokenOther
    13 -> case lst of
        100 -> if bs === "last-modified" then tokenLastModified else tokenOther
        101 -> if bs === "content-range" then tokenContentRange else tokenOther
        104 -> if bs === "if-none-match" then tokenIfNoneMatch else tokenOther
        108 -> if bs === "cache-control" then tokenCacheControl else tokenOther
        110 -> if bs === "authorization" then tokenAuthorization else tokenOther
        115 -> if bs === "accept-ranges" then tokenAcceptRanges else tokenOther
        _   -> tokenOther
    14 -> case lst of
        104 -> if bs === "content-length" then tokenContentLength else tokenOther
        116 -> if bs === "accept-charset" then tokenAcceptCharset else tokenOther
        _   -> tokenOther
    15 -> case lst of
        101 -> if bs === "accept-language" then tokenAcceptLanguage else tokenOther
        103 -> if bs === "accept-encoding" then tokenAcceptEncoding else tokenOther
        _   -> tokenOther
    16 -> case lst of
        101 -> if bs === "content-language" then tokenContentLanguage else
               if bs === "www-authenticate" then tokenWwwAuthenticate else tokenOther
        103 -> if bs === "content-encoding" then tokenContentEncoding else tokenOther
        110 -> if bs === "content-location" then tokenContentLocation else tokenOther
        _   -> tokenOther
    17 -> case lst of
        101 -> if bs === "if-modified-since" then tokenIfModifiedSince else tokenOther
        103 -> if bs === "transfer-encoding" then tokenTransferEncoding else tokenOther
        _   -> tokenOther
    18 -> case lst of
        101 -> if bs === "proxy-authenticate" then tokenProxyAuthenticate else tokenOther
        _   -> tokenOther
    19 -> case lst of
        101 -> if bs === "if-unmodified-since" then tokenIfUnmodifiedSince else tokenOther
        110 -> if bs === "proxy-authorization" then tokenProxyAuthorization else
               if bs === "content-disposition" then tokenContentDisposition else tokenOther
        _   -> tokenOther
    25 -> case lst of
        121 -> if bs === "strict-transport-security" then tokenStrictTransportSecurity else tokenOther
        _   -> tokenOther
    27 -> case lst of
        110 -> if bs === "access-control-allow-origin" then tokenAccessControlAllowOrigin else tokenOther
        _   -> tokenOther
    _ -> tokenOther
  where
    len = B.length bs
    lst = B.last bs
    PS fp1 off1 siz === PS fp2 off2 _ = unsafeDupablePerformIO $
      withForeignPtr fp1 $ \p1 ->
      withForeignPtr fp2 $ \p2 -> do
        i <- memcmp (p1 `plusPtr` off1) (p2 `plusPtr` off2) siz
        return $! i == 0
