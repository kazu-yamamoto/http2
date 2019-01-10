{-# LANGUAGE OverloadedStrings #-}

module HPACK.HeaderBlock where

import Data.ByteString (ByteString)
import Data.ByteString.Base16
import Network.HPACK.Types

fromHexString :: ByteString -> ByteString
fromHexString = fst . decode

----------------------------------------------------------------

d41h :: HeaderList
d41h = [(":method","GET")
       ,(":scheme","http")
       ,(":path","/")
       ,(":authority","www.example.com")
       ]

d41b :: ByteString
d41b = fromHexString "828684418cf1e3c2e5f23a6ba0ab90f4ff"

d42h :: HeaderList
d42h = [(":method","GET")
       ,(":scheme","http")
       ,(":path","/")
       ,(":authority","www.example.com")
       ,("cache-control","no-cache")]

d42b :: ByteString
d42b = fromHexString "828684be5886a8eb10649cbf"

d43h :: HeaderList
d43h = [(":method","GET")
       ,(":scheme","https")
       ,(":path","/index.html")
       ,(":authority","www.example.com")
       ,("custom-key","custom-value")
       ]

d43b :: ByteString
d43b = fromHexString "828785bf408825a849e95ba97d7f8925a849e95bb8e8b4bf"

----------------------------------------------------------------

d61h :: HeaderList
d61h = [(":status","302")
       ,("cache-control","private")
       ,("date","Mon, 21 Oct 2013 20:13:21 GMT")
       ,("location","https://www.example.com")
       ]

d61b :: ByteString
d61b = fromHexString "488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3"

d62h :: HeaderList
d62h = [(":status","307")
       ,("cache-control","private")
       ,("date","Mon, 21 Oct 2013 20:13:21 GMT")
       ,("location","https://www.example.com")
       ]

d62b :: ByteString
d62b = fromHexString "4883640effc1c0bf"

d63h :: HeaderList
d63h = [(":status","200")
       ,("cache-control","private")
       ,("date","Mon, 21 Oct 2013 20:13:22 GMT")
       ,("location","https://www.example.com")
       ,("content-encoding","gzip")
       ,("set-cookie","foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1")]

d63b :: ByteString
d63b = fromHexString "88c16196d07abe941054d444a8200595040b8166e084a62d1bffc05a839bd9ab77ad94e7821dd7f2e6c7b335dfdfcd5b3960d5af27087f3672c1ab270fb5291f9587316065c003ed4ee5b1063d5007"
