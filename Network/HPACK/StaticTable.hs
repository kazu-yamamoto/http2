{-# LANGUAGE OverloadedStrings #-}

module Network.HPACK.StaticTable (
    Table
  , StaticTable(..)
  , staticTable
    ) where

import Data.Array (Array, listArray)
import Network.HPACK.Entry
import Network.HPACK.Types

type Table = Array Index Entry

data StaticTable = StaticTable Size Table deriving Show

staticTable :: StaticTable
staticTable = StaticTable 60 $ listArray (1,60) $ map toEntry staticTableList

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
