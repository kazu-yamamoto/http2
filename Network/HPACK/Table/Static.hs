{-# LANGUAGE OverloadedStrings #-}

module Network.HPACK.Table.Static (
    staticTableSize
  , toStaticEntry
  , toStaticIndex
  , toStaticIndexValue
  ) where

import Data.Array (Array, listArray, (!))
import Data.HashTable.IO (BasicHashTable)
import qualified Data.HashTable.IO as I
import Network.HPACK.Table.Entry
import Network.HPACK.Types
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Char8 as H

----------------------------------------------------------------

-- | The size of static table.
staticTableSize :: Size
staticTableSize = 60

-- | Get 'Entry' from the static table.
--
-- >>> toStaticEntry 8
-- (42,(":status","200"))
-- >>> toStaticEntry 49
-- (37,("range",""))
toStaticEntry :: Index -> Entry
toStaticEntry idx = staticTable ! idx

-- | Pre-defined static table.
staticTable :: Array Index Entry
staticTable = listArray (1,60) $ map toEntry staticTableList

----------------------------------------------------------------

-- | Get 'Index' from the static table.
--   Only non colon headers.
--
-- >>> toStaticIndex "date"
-- Just 32
-- >>> toStaticIndex "user-agent"
-- Just 57
toStaticIndex :: HeaderName -> IO (Maybe Index)
toStaticIndex k = I.lookup staticHashTable k

staticHashTable :: BasicHashTable HeaderName Index
staticHashTable = unsafePerformIO $ I.fromList alist
  where
    beg = length staticVirtualHeaderList + 1
    alist = zip staticKeyList [beg ..]

----------------------------------------------------------------

-- | Get 'Index' from the static table.
--   Only colon headers.
--
-- >>> toStaticIndexValue (":path","/index.html")
-- Just 5
-- >>> toStaticIndexValue (":status","200")
-- Just 8
toStaticIndexValue :: Header -> IO (Maybe Index)
toStaticIndexValue h = I.lookup staticColonHashTable h

staticColonHashTable :: BasicHashTable Header Index
staticColonHashTable = unsafePerformIO $ I.fromList alist
  where
    alist = zip staticVirtualHeaderList [1 ..]

----------------------------------------------------------------

staticVirtualHeaderList :: [Header]
staticKeyList :: [HeaderName]

(staticVirtualHeaderList,staticKeyList) = (vs,ks)
  where
    isColon e = H.head (fromHeaderName (fst e)) == ':'
    (vs,ks') = span isColon staticTableList
    ks = map fst ks'

----------------------------------------------------------------

staticTableList :: [Header]
staticTableList = [
    (":authority","")
  , (":method","GET")
  , (":method","POST")
  , (":path","/")
  , (":path","/index.html")
  , (":scheme","http")
  , (":scheme","https")
  , (":status","200")
  , (":status","500")
  , (":status","404")
  , (":status","403")
  , (":status","400")
  , (":status","401")
  , ("accept-charset","")
  , ("accept-encoding","")
  , ("accept-language","")
  , ("accept-ranges","")
  , ("accept","")
  , ("access-control-allow-origin","")
  , ("age","")
  , ("allow","")
  , ("authorization","")
  , ("cache-control","")
  , ("content-disposition","")
  , ("content-encoding","")
  , ("content-language","")
  , ("content-length","")
  , ("content-location","")
  , ("content-range","")
  , ("content-type","")
  , ("cookie","")
  , ("date","")
  , ("etag","")
  , ("expect","")
  , ("expires","")
  , ("from","")
  , ("host","")
  , ("if-match","")
  , ("if-modified-since","")
  , ("if-none-match","")
  , ("if-range","")
  , ("if-unmodified-since","")
  , ("last-modified","")
  , ("link","")
  , ("location","")
  , ("max-forwards","")
  , ("proxy-authenticate","")
  , ("proxy-authorization","")
  , ("range","")
  , ("referer","")
  , ("refresh","")
  , ("retry-after","")
  , ("server","")
  , ("set-cookie","")
  , ("strict-transport-security","")
  , ("transfer-encoding","")
  , ("user-agent","")
  , ("vary","")
  , ("via","")
  , ("www-authenticate","")
  ]
