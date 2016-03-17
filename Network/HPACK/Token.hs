{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Network.HPACK.Token where

import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(..), memcmp)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.CaseInsensitive (original, mk, CI(..))

-- $setup
-- >>> :set -XOverloadedStrings

data Token = Token {
    ix :: !Int
  , shouldBeIndexed :: !Bool -- should be indexed
  , isPseudo :: !Bool -- is this a pseudo header key?
  , tokenKey :: !(CI ByteString)
  } deriving (Eq, Show)

{-# INLINE toIx #-}
toIx :: Token -> Int
toIx (Token n _ _ _) = n

{-# INLINE tokenOriginalKey #-}
tokenOriginalKey :: Token -> ByteString
tokenOriginalKey (Token _ _ _ ci) = original ci

{-# INLINE tokenFoldedKey #-}
tokenFoldedKey :: Token -> ByteString
tokenFoldedKey (Token _ _ _ ci) = foldedCase ci

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
tokenConnection               :: Token -- Original
tokenTE                       :: Token -- Original
tokenDummy                    :: Token

tokenAuthority                = Token  0  True  True ":authority"
tokenMethod                   = Token  1  True  True ":method"
tokenPath                     = Token  2 False  True ":path"
tokenScheme                   = Token  3  True  True ":scheme"
tokenStatus                   = Token  4  True  True ":status"
tokenAcceptCharset            = Token  5  True False "Accept-Charset"
tokenAcceptEncoding           = Token  6  True False "Accept-Encoding"
tokenAcceptLanguage           = Token  7  True False "Accept-Language"
tokenAcceptRanges             = Token  8  True False "Accept-Ranges"
tokenAccept                   = Token  9  True False "Accept"
tokenAccessControlAllowOrigin = Token 10  True False "Access-Control-Allow-Origin"
tokenAge                      = Token 11  True False "Age"
tokenAllow                    = Token 12  True False "Allow"
tokenAuthorization            = Token 13  True False "Authorization"
tokenCacheControl             = Token 14  True False "Cache-Control"
tokenContentDisposition       = Token 15  True False "Content-Disposition"
tokenContentEncoding          = Token 16  True False "Content-Encoding"
tokenContentLanguage          = Token 17  True False "Content-Language"
tokenContentLength            = Token 18 False False "Content-Length"
tokenContentLocation          = Token 19 False False "Content-Location"
tokenContentRange             = Token 20  True False "Content-Range"
tokenContentType              = Token 21  True False "Content-Type"
tokenCookie                   = Token 22  True False "Cookie"
tokenDate                     = Token 23  True False "Date"
tokenEtag                     = Token 24 False False "Etag"
tokenExpect                   = Token 25  True False "Expect"
tokenExpires                  = Token 26  True False "Expires"
tokenFrom                     = Token 27  True False "From"
tokenHost                     = Token 28  True False "Host"
tokenIfMatch                  = Token 29  True False "If-Match"
tokenIfModifiedSince          = Token 30  True False "If-Modified-Since"
tokenIfNoneMatch              = Token 31  True False "If-None-Match"
tokenIfRange                  = Token 32  True False "If-Range"
tokenIfUnmodifiedSince        = Token 33  True False "If-Unmodified-Since"
tokenLastModified             = Token 34  True False "Last-Modified"
tokenLink                     = Token 35  True False "Link"
tokenLocation                 = Token 36  True False "Location"
tokenMaxForwards              = Token 37  True False "Max-Forwards"
tokenProxyAuthenticate        = Token 38  True False "Proxy-Authenticate"
tokenProxyAuthorization       = Token 39  True False "Proxy-Authorization"
tokenRange                    = Token 40  True False "Range"
tokenReferer                  = Token 41  True False "Referer"
tokenRefresh                  = Token 42  True False "Refresh"
tokenRetryAfter               = Token 43  True False "Retry-After"
tokenServer                   = Token 44  True False "Server"
tokenSetCookie                = Token 45 False False "Set-Cookie"
tokenStrictTransportSecurity  = Token 46  True False "Strict-Transport-Security"
tokenTransferEncoding         = Token 47  True False "Transfer-Encoding"
tokenUserAgent                = Token 48  True False "User-Agent"
tokenVary                     = Token 49  True False "Vary"
tokenVia                      = Token 50  True False "Via"
tokenWwwAuthenticate          = Token 51  True False "Www-Authenticate"
tokenConnection               = Token 52 False False "Connection"
tokenTE                       = Token 53 False False "TE"
tokenDummy                    = Token 54  True False "dummy"

minToken :: Int
minToken = 0

maxToken :: Int
maxToken = 53

staticToken :: Int
staticToken = 51

otherToken :: Int
otherToken = maxToken + 1

{-# INLINE isIxOther #-}
isIxOther :: Int -> Bool
isIxOther n = n == otherToken

{-# INLINE isIxNonStatic #-}
isIxNonStatic :: Int -> Bool
isIxNonStatic n = n > staticToken

{-# INLINE isTokenNonStatic #-}
isTokenNonStatic :: Token -> Bool
isTokenNonStatic n = toIx n > staticToken

-- |
--
-- >>> toToken ":authority" == tokenAuthority
-- True
-- >>> toToken "foo"
-- Token {ix = 54, shouldBeIndexed = True, isPseudo = False, tokenKey = "foo"}
-- >>> toToken ":bar"
-- Token {ix = 54, shouldBeIndexed = True, isPseudo = True, tokenKey = ":bar"}
toToken :: ByteString -> Token
toToken bs = case len of
    2 -> if bs == "te" then tokenTE else mkTokenOther bs
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
        116
          | bs === "expect" -> tokenExpect
          | bs === "accept" -> tokenAccept
          | otherwise       -> mkTokenOther bs
        _   -> mkTokenOther bs
    7 -> case lst of
        100 -> if bs === ":method" then tokenMethod else mkTokenOther bs
        101 -> if bs === ":scheme" then tokenScheme else mkTokenOther bs
        104 -> if bs === "refresh" then tokenRefresh else mkTokenOther bs
        114 -> if bs === "referer" then tokenReferer else mkTokenOther bs
        115
          | bs === "expires" -> tokenExpires
          | bs === ":status" -> tokenStatus
          | otherwise        -> mkTokenOther bs
        _   -> mkTokenOther bs
    8 -> case lst of
        101 -> if bs === "if-range" then tokenIfRange else mkTokenOther bs
        104 -> if bs === "if-match" then tokenIfMatch else mkTokenOther bs
        110 -> if bs === "location" then tokenLocation else mkTokenOther bs
        _   -> mkTokenOther bs
    10 -> case lst of
        101 -> if bs === "set-cookie" then tokenSetCookie else mkTokenOther bs
        110 -> if bs === "connection" then tokenConnection else mkTokenOther bs
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
        101
          | bs === "content-language" -> tokenContentLanguage
          | bs === "www-authenticate" -> tokenWwwAuthenticate
          | otherwise                 -> mkTokenOther bs
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
        110
          | bs === "proxy-authorization" -> tokenProxyAuthorization
          | bs === "content-disposition" -> tokenContentDisposition
          | otherwise                    -> mkTokenOther bs
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
mkTokenOther bs = Token otherToken True p (mk bs)
  where
    !p | B.length bs == 0 = False
       | B.head bs == 58  = True
       | otherwise        = False

