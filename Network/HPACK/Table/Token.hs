{-# LANGUAGE OverloadedStrings #-}

module Network.HPACK.Table.Token where

import Data.Ix
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

-- $setup
-- >>> :set -XOverloadedStrings

data Token = TAuthority
           | TMethod
           | TPath
           | TScheme
           | TStatus
           | TAcceptCharset
           | TAcceptEncoding
           | TAcceptLanguage
           | TAcceptRanges
           | TAccept
           | TAccessControlAllowOrigin
           | TAge
           | TAllow
           | TAuthorization
           | TCacheControl
           | TContentDisposition
           | TContentEncoding
           | TContentLanguage
           | TContentLength
           | TContentLocation
           | TContentRange
           | TContentType
           | TCookie
           | TDate
           | TEtag
           | TExpect
           | TExpires
           | TFrom
           | THost
           | TIfMatch
           | TIfModifiedSince
           | TIfNoneMatch
           | TIfRange
           | TIfUnmodifiedSince
           | TLastModified
           | TLink
           | TLocation
           | TMaxForwards
           | TProxyAuthenticate
           | TProxyAuthorization
           | TRange
           | TReferer
           | TRefresh
           | TRetryAfter
           | TServer
           | TSetCookie
           | TStrictTransportSecurity
           | TTransferEncoding
           | TUserAgent
           | TVary
           | TVia
           | TWwwAuthenticate
           | TOTHER
           deriving (Eq,Ord,Show,Enum,Bounded,Ix)

-- |
--
-- >>> toToken ":authority"
-- TAuthority
-- >>> toToken ":method"
-- TMethod
-- >>> toToken ":path"
-- TPath
-- >>> toToken ":scheme"
-- TScheme
-- >>> toToken ":status"
-- TStatus
-- >>> toToken "accept-charset"
-- TAcceptCharset
-- >>> toToken "accept-encoding"
-- TAcceptEncoding
-- >>> toToken "accept-language"
-- TAcceptLanguage
-- >>> toToken "accept-ranges"
-- TAcceptRanges
-- >>> toToken "accept"
-- TAccept
-- >>> toToken "access-control-allow-origin"
-- TAccessControlAllowOrigin
-- >>> toToken "age"
-- TAge
-- >>> toToken "allow"
-- TAllow
-- >>> toToken "authorization"
-- TAuthorization
-- >>> toToken "cache-control"
-- TCacheControl
-- >>> toToken "content-disposition"
-- TContentDisposition
-- >>> toToken "content-encoding"
-- TContentEncoding
-- >>> toToken "content-language"
-- TContentLanguage
-- >>> toToken "content-length"
-- TContentLength
-- >>> toToken "content-location"
-- TContentLocation
-- >>> toToken "content-range"
-- TContentRange
-- >>> toToken "content-type"
-- TContentType
-- >>> toToken "cookie"
-- TCookie
-- >>> toToken "date"
-- TDate
-- >>> toToken "etag"
-- TEtag
-- >>> toToken "expect"
-- TExpect
-- >>> toToken "expires"
-- TExpires
-- >>> toToken "from"
-- TFrom
-- >>> toToken "host"
-- THost
-- >>> toToken "if-match"
-- TIfMatch
-- >>> toToken "if-modified-since"
-- TIfModifiedSince
-- >>> toToken "if-none-match"
-- TIfNoneMatch
-- >>> toToken "if-range"
-- TIfRange
-- >>> toToken "if-unmodified-since"
-- TIfUnmodifiedSince
-- >>> toToken "last-modified"
-- TLastModified
-- >>> toToken "link"
-- TLink
-- >>> toToken "location"
-- TLocation
-- >>> toToken "max-forwards"
-- TMaxForwards
-- >>> toToken "proxy-authenticate"
-- TProxyAuthenticate
-- >>> toToken "proxy-authorization"
-- TProxyAuthorization
-- >>> toToken "range"
-- TRange
-- >>> toToken "referer"
-- TReferer
-- >>> toToken "refresh"
-- TRefresh
-- >>> toToken "retry-after"
-- TRetryAfter
-- >>> toToken "server"
-- TServer
-- >>> toToken "set-cookie"
-- TSetCookie
-- >>> toToken "strict-transport-security"
-- TStrictTransportSecurity
-- >>> toToken "transfer-encoding"
-- TTransferEncoding
-- >>> toToken "user-agent"
-- TUserAgent
-- >>> toToken "vary"
-- TVary
-- >>> toToken "via"
-- TVia
-- >>> toToken "www-authenticate"
-- TWwwAuthenticate
-- >>> toToken "foo"
-- TOTHER
toToken :: ByteString -> Token
toToken bs = case len of
    3 -> case lst of
        97  -> if bs == "via" then TVia else TOTHER
        101 -> if bs == "age" then TAge else TOTHER
        _   -> TOTHER
    4 -> case lst of
        101 -> if bs == "date" then TDate else TOTHER
        103 -> if bs == "etag" then TEtag else TOTHER
        107 -> if bs == "link" then TLink else TOTHER
        109 -> if bs == "from" then TFrom else TOTHER
        116 -> if bs == "host" then THost else TOTHER
        121 -> if bs == "vary" then TVary else TOTHER
        _   -> TOTHER
    5 -> case lst of
        101 -> if bs == "range" then TRange else TOTHER
        104 -> if bs == ":path" then TPath else TOTHER
        119 -> if bs == "allow" then TAllow else TOTHER
        _   -> TOTHER
    6 -> case lst of
        101 -> if bs == "cookie" then TCookie else TOTHER
        114 -> if bs == "server" then TServer else TOTHER
        116 -> if bs == "expect" then TExpect else
               if bs == "accept" then TAccept else TOTHER
        _   -> TOTHER
    7 -> case lst of
        100 -> if bs == ":method" then TMethod else TOTHER
        101 -> if bs == ":scheme" then TScheme else TOTHER
        104 -> if bs == "refresh" then TRefresh else TOTHER
        114 -> if bs == "referer" then TReferer else TOTHER
        115 -> if bs == "expires" then TExpires else
               if bs == ":status" then TStatus else TOTHER
        _   -> TOTHER
    8 -> case lst of
        101 -> if bs == "if-range" then TIfRange else TOTHER
        104 -> if bs == "if-match" then TIfMatch else TOTHER
        110 -> if bs == "location" then TLocation else TOTHER
        _   -> TOTHER
    10 -> case lst of
        101 -> if bs == "set-cookie" then TSetCookie else TOTHER
        116 -> if bs == "user-agent" then TUserAgent else TOTHER
        121 -> if bs == ":authority" then TAuthority else TOTHER
        _   -> TOTHER
    11 -> case lst of
        114 -> if bs == "retry-after" then TRetryAfter else TOTHER
        _   -> TOTHER
    12 -> case lst of
        101 -> if bs == "content-type" then TContentType else TOTHER
        115 -> if bs == "max-forwards" then TMaxForwards else TOTHER
        _   -> TOTHER
    13 -> case lst of
        100 -> if bs == "last-modified" then TLastModified else TOTHER
        101 -> if bs == "content-range" then TContentRange else TOTHER
        104 -> if bs == "if-none-match" then TIfNoneMatch else TOTHER
        108 -> if bs == "cache-control" then TCacheControl else TOTHER
        110 -> if bs == "authorization" then TAuthorization else TOTHER
        115 -> if bs == "accept-ranges" then TAcceptRanges else TOTHER
        _   -> TOTHER
    14 -> case lst of
        104 -> if bs == "content-length" then TContentLength else TOTHER
        116 -> if bs == "accept-charset" then TAcceptCharset else TOTHER
        _   -> TOTHER
    15 -> case lst of
        101 -> if bs == "accept-language" then TAcceptLanguage else TOTHER
        103 -> if bs == "accept-encoding" then TAcceptEncoding else TOTHER
        _   -> TOTHER
    16 -> case lst of
        101 -> if bs == "content-language" then TContentLanguage else
               if bs == "www-authenticate" then TWwwAuthenticate else TOTHER
        103 -> if bs == "content-encoding" then TContentEncoding else TOTHER
        110 -> if bs == "content-location" then TContentLocation else TOTHER
        _   -> TOTHER
    17 -> case lst of
        101 -> if bs == "if-modified-since" then TIfModifiedSince else TOTHER
        103 -> if bs == "transfer-encoding" then TTransferEncoding else TOTHER
        _   -> TOTHER
    18 -> case lst of
        101 -> if bs == "proxy-authenticate" then TProxyAuthenticate else TOTHER
        _   -> TOTHER
    19 -> case lst of
        101 -> if bs == "if-unmodified-since" then TIfUnmodifiedSince else TOTHER
        110 -> if bs == "proxy-authorization" then TProxyAuthorization else
               if bs == "content-disposition" then TContentDisposition else TOTHER
        _   -> TOTHER
    25 -> case lst of
        121 -> if bs == "strict-transport-security" then TStrictTransportSecurity else TOTHER
        _   -> TOTHER
    27 -> case lst of
        110 -> if bs == "access-control-allow-origin" then TAccessControlAllowOrigin else TOTHER
        _   -> TOTHER
    _ -> TOTHER
  where
    len = B.length bs
    lst = B.last bs
