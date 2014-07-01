{-# LANGUAGE OverloadedStrings #-}

module HeaderBlock where

import Network.HPACK.Context
import Network.HPACK.HeaderBlock
import Network.HPACK.Types

import HexString

----------------------------------------------------------------

d41 :: HeaderBlock
d41 = [
    Indexed 2
  , Indexed 7
  , Indexed 6
  , Literal Add (Idx 4) "www.example.com"
  ]

d41h :: HeaderSet
d41h = [(":method","GET")
        ,(":scheme","http")
        ,(":path","/")
        ,(":authority","www.example.com")
        ]

d41b :: ByteStream
d41b = fromHexString "828786448cf1e3c2e5f23a6ba0ab90f4ff"

d42 :: HeaderBlock
d42 = [
    Literal Add (Idx 28) "no-cache"
  ]

d42h :: HeaderSet
d42h = [("cache-control","no-cache")
        ,(":method","GET")
        ,(":scheme","http")
        ,(":path","/")
        ,(":authority","www.example.com")]

d42b :: ByteStream
d42b = fromHexString "5c86a8eb10649cbf"

d43 :: HeaderBlock
d43 = [
    Clear
  , Indexed 5
  , Indexed 12
  , Indexed 11
  , Indexed 4
  , Literal Add (Lit "custom-key") "custom-value"
  ]

d43h :: HeaderSet
d43h = [(":method","GET")
        ,(":scheme","https")
        ,(":path","/index.html")
        ,(":authority","www.example.com")
        ,("custom-key","custom-value")
        ]

d43b :: ByteStream
d43b = fromHexString "30858c8b84408825a849e95ba97d7f8925a849e95bb8e8b4bf"

----------------------------------------------------------------

d61 :: HeaderBlock
d61 = [
    Literal Add (Idx 8) "302"
  , Literal Add (Idx 25) "private"
  , Literal Add (Idx 35) "Mon, 21 Oct 2013 20:13:21 GMT"
  , Literal Add (Idx 49) "https://www.example.com"
  ]

d61h :: HeaderSet
d61h = [(":status","302")
        ,("cache-control","private")
        ,("date","Mon, 21 Oct 2013 20:13:21 GMT")
        ,("location","https://www.example.com")
        ]

d61b :: ByteStream
d61b = fromHexString "488264025985aec3771a4b6396d07abe941054d444a8200595040b8166e082a62d1bff71919d29ad171863c78f0b97c8e9ae82ae43d3"

d62 :: HeaderBlock
d62 = [
    Indexed 12
  ]

d62h :: HeaderSet
d62h = [(":status","200")
        ,("cache-control","private")
        ,("date","Mon, 21 Oct 2013 20:13:21 GMT")
        ,("location","https://www.example.com")
        ]

d62b :: ByteStream
d62b = fromHexString "8c"

d63 :: HeaderBlock
d63 = [
    Indexed 4
  , Indexed 4
  , Literal Add (Idx 3) "Mon, 21 Oct 2013 20:13:22 GMT"
  , Literal Add (Idx 30) "gzip"
  , Indexed 4
  , Indexed 4
  , Indexed 3
  , Indexed 3
  ,Literal Add (Idx 59) "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"
  ]

d63h :: HeaderSet
d63h = [("cache-control","private")
        ,("date","Mon, 21 Oct 2013 20:13:22 GMT")
        ,("content-encoding","gzip")
        ,("location","https://www.example.com")
        ,(":status","200")
        ,("set-cookie","foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1")]

d63b :: ByteStream
d63b = fromHexString "84844396d07abe941054d444a8200595040b8166e084a62d1bff5e839bd9ab848483837bad94e7821dd7f2e6c7b335dfdfcd5b3960d5af27087f3672c1ab270fb5291f9587316065c003ed4ee5b1063d5007"
