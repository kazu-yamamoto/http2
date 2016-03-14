{-# LANGUAGE OverloadedStrings #-}

module Network.HPACK.Table.Static (
    toStaticEntry
  , staticTableSize
  , staticTableList
  ) where

import Data.Array (Array, listArray, (!))
import Network.HPACK.Table.Entry

----------------------------------------------------------------

-- | The size of static table.
staticTableSize :: Size
staticTableSize = length staticTableList

-- fixme
{-# INLINE toStaticEntry #-}
-- | Get 'Entry' from the static table.
--
-- >>> toStaticEntry 1
-- Entry 42 (Token 0 True ":authority") ""
-- >>> toStaticEntry 8
-- Entry 42 (Token 4 True ":status") "200"
-- >>> toStaticEntry 50
-- Entry 37 (Token 40 True "range") ""
toStaticEntry :: Index -> Entry
toStaticEntry sidx = staticTable ! sidx

-- | Pre-defined static table.
staticTable :: Array Index Entry
staticTable = listArray (1,staticTableSize) $ map toEntry staticTableList

----------------------------------------------------------------

staticTableList :: [Header]
staticTableList = [
    (":authority", "")
  , (":method", "GET")
  , (":method", "POST")
  , (":path", "/")
  , (":path", "/index.html")
  , (":scheme", "http")
  , (":scheme", "https")
  , (":status", "200")
  , (":status", "204")
  , (":status", "206")
  , (":status", "304")
  , (":status", "400")
  , (":status", "404")
  , (":status", "500")
  , ("accept-charset", "")
  , ("accept-encoding", "gzip, deflate")
  , ("accept-language", "")
  , ("accept-ranges", "")
  , ("accept", "")
  , ("access-control-allow-origin", "")
  , ("age", "")
  , ("allow", "")
  , ("authorization", "")
  , ("cache-control", "")
  , ("content-disposition", "")
  , ("content-encoding", "")
  , ("content-language", "")
  , ("content-length", "")
  , ("content-location", "")
  , ("content-range", "")
  , ("content-type", "")
  , ("cookie", "")
  , ("date", "")
  , ("etag", "")
  , ("expect", "")
  , ("expires", "")
  , ("from", "")
  , ("host", "")
  , ("if-match", "")
  , ("if-modified-since", "")
  , ("if-none-match", "")
  , ("if-range", "")
  , ("if-unmodified-since", "")
  , ("last-modified", "")
  , ("link", "")
  , ("location", "")
  , ("max-forwards", "")
  , ("proxy-authenticate", "")
  , ("proxy-authorization", "")
  , ("range", "")
  , ("referer", "")
  , ("refresh", "")
  , ("retry-after", "")
  , ("server", "")
  , ("set-cookie", "")
  , ("strict-transport-security", "")
  , ("transfer-encoding", "")
  , ("user-agent", "")
  , ("vary", "")
  , ("via", "")
  , ("www-authenticate", "")
  ]
