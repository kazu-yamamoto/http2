{-# LANGUAGE OverloadedStrings #-}

module Network.HPACK.Table.Static (
    SIndex(..)
  , fromStaticIndex
  , toStaticIndex
  , isSIndexValid
  , toStaticEntry
  , staticHashPSQ
  ) where

import Data.Array (Array, listArray, (!))
import Network.HPACK.Table.Entry
import qualified Network.HPACK.Table.HashPSQ as HP

----------------------------------------------------------------

newtype SIndex = SIndex Int deriving (Eq,Ord,Show)

fromStaticIndex :: SIndex -> Int
fromStaticIndex (SIndex sidx) = sidx

toStaticIndex :: Int -> SIndex
toStaticIndex = SIndex

isSIndexValid :: SIndex -> Bool
isSIndexValid (SIndex sidx) = 1 <= sidx && sidx <= staticTableSize

----------------------------------------------------------------

-- | The size of static table.
staticTableSize :: Size
staticTableSize = 60

-- | Get 'Entry' from the static table.
--
-- >>> toStaticEntry (SIndex 8)
-- (42,(":status","200"))
-- >>> toStaticEntry (SIndex 50)
-- (37,("range",""))
toStaticEntry :: SIndex -> Entry
toStaticEntry (SIndex sidx) = staticTable ! sidx

-- | Pre-defined static table.
staticTable :: Array Index Entry
staticTable = listArray (1,60) $ map toEntry staticTableList

staticHashPSQ :: HP.HashPSQ SIndex
staticHashPSQ = HP.fromList alist
  where
    is = map toStaticIndex [1..]
    alist = zip is staticTableList

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
  , (":status","204")
  , (":status","206")
  , (":status","304")
  , (":status","400")
  , (":status","404")
  , (":status","500")
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
