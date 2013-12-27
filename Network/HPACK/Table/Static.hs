{-# LANGUAGE OverloadedStrings #-}

module Network.HPACK.Table.Static (
    staticTableSize
  , toStaticEntry
  , toStaticIndex
  , toStaticColonIndex
  , isColon
  ) where

import Data.Array (Array, listArray, (!))
import Data.HashTable.IO (BasicHashTable)
import qualified Data.HashTable.IO as I
import Network.HPACK.Table.Entry
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
--
-- >>> toStaticIndex ":status"
-- Just 13
-- >>> toStaticIndex "date"
-- Just 32
-- >>> toStaticIndex "user-agent"
-- Just 57
toStaticIndex :: HeaderName -> IO (Maybe Index)
toStaticIndex k = I.lookup staticHashTable k

staticHashTable :: BasicHashTable HeaderName Index
staticHashTable = unsafePerformIO $ I.fromList alist
  where
    alist = zip (map fst staticTableList) [1 ..]

----------------------------------------------------------------

-- | Get 'Index' from the static table.
--   Only colon headers.
--
-- >>> toStaticColonIndex (":path","/index.html")
-- Just 5
-- >>> toStaticColonIndex (":status","200")
-- Just 8
toStaticColonIndex :: Header -> IO (Maybe Index)
toStaticColonIndex h = I.lookup staticColonHashTable h

staticColonHashTable :: BasicHashTable Header Index
staticColonHashTable = unsafePerformIO $ I.fromList alist
  where
    alist = zip staticColonHeaderList [1 ..]

----------------------------------------------------------------

staticColonHeaderList :: [Header]

staticColonHeaderList = takeWhile isColon staticTableList

-- | Checking if 'HeaderName' starts with colon.
isColon :: Header -> Bool
isColon h = H.head (fst h) == ':'

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
