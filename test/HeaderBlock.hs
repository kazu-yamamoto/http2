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
  , Indexed 6
  , Indexed 4
  , Literal Add (Idx 1) "www.example.com"
  ]

d41h :: HeaderSet
d41h = [(":method","GET")
       ,(":scheme","http")
       ,(":path","/")
       ,(":authority","www.example.com")
       ]

d41b :: ByteStream
d41b = fromHexString "828684418cf1e3c2e5f23a6ba0ab90f4ff"

d42 :: HeaderBlock
d42 = [
    Indexed 2
  , Indexed 6
  , Indexed 4
  , Indexed 62
  , Literal Add (Idx 24) "no-cache"
  ]

d42h :: HeaderSet
d42h = [(":method","GET")
       ,(":scheme","http")
       ,(":path","/")
       ,(":authority","www.example.com")
       ,("cache-control","no-cache")]

d42b :: ByteStream
d42b = fromHexString "828684be5886a8eb10649cbf"

d43 :: HeaderBlock
d43 = [
    Indexed 2
  , Indexed 7
  , Indexed 5
  , Indexed 63
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
d43b = fromHexString "828785bf408825a849e95ba97d7f8925a849e95bb8e8b4bf"

----------------------------------------------------------------

d61 :: HeaderBlock
d61 = [
    Literal Add (Idx 8) "302"
  , Literal Add (Idx 24) "private"
  , Literal Add (Idx 33) "Mon, 21 Oct 2013 20:13:21 GMT"
  , Literal Add (Idx 46) "https://www.example.com"
  ]

d61h :: HeaderSet
d61h = [(":status","302")
       ,("cache-control","private")
       ,("date","Mon, 21 Oct 2013 20:13:21 GMT")
       ,("location","https://www.example.com")
       ]

d61b :: ByteStream
d61b = fromHexString "488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3"

d62 :: HeaderBlock
d62 = [
    Literal Add (Idx 8) "307"
  , Indexed 65
  , Indexed 64
  , Indexed 63
  ]

d62h :: HeaderSet
d62h = [(":status","307")
       ,("cache-control","private")
       ,("date","Mon, 21 Oct 2013 20:13:21 GMT")
       ,("location","https://www.example.com")
       ]

d62b :: ByteStream
d62b = fromHexString "4883640effc1c0bf"

d63 :: HeaderBlock
d63 = [
    Indexed 8
  , Indexed 65
  , Literal Add (Idx 33) "Mon, 21 Oct 2013 20:13:22 GMT"
  , Indexed 64
  , Literal Add (Idx 26) "gzip"
  ,Literal Add (Idx 55) "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"
  ]

d63h :: HeaderSet
d63h = [(":status","200")
       ,("cache-control","private")
       ,("date","Mon, 21 Oct 2013 20:13:22 GMT")
       ,("location","https://www.example.com")
       ,("content-encoding","gzip")
       ,("set-cookie","foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1")]

d63b :: ByteStream
d63b = fromHexString "88c16196d07abe941054d444a8200595040b8166e084a62d1bffc05a839bd9ab77ad94e7821dd7f2e6c7b335dfdfcd5b3960d5af27087f3672c1ab270fb5291f9587316065c003ed4ee5b1063d5007"
