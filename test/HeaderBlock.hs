{-# LANGUAGE OverloadedStrings #-}

module HeaderBlock where

import Network.HPACK.Context
import Network.HPACK.HeaderBlock

----------------------------------------------------------------

e211 :: HeaderBlock
e211 = [
    Indexed 2
  , Indexed 7
  , Indexed 6
  , Literal Add (Idx 4) "www.example.com"
  ]

e211h :: HeaderSet
e211h = [(":method","GET")
        ,(":scheme","http")
        ,(":path","/")
        ,(":authority","www.example.com")
        ]

e212 :: HeaderBlock
e212 = [
    Literal Add (Idx 27) "no-cache"
  ]

e212h :: HeaderSet
e212h = [("cache-control","no-cache")
        ,(":method","GET")
        ,(":scheme","http")
        ,(":path","/")
        ,(":authority","www.example.com")]

e213 :: HeaderBlock
e213 = [
    Indexed 0
  , Indexed 5
  , Indexed 12
  , Indexed 11
  , Indexed 4
  , Literal Add (Lit "custom-key") "custom-value"
  ]

e213h :: HeaderSet
e213h = [(":method","GET")
        ,(":scheme","https")
        ,(":path","/index.html")
        ,(":authority","www.example.com")
        ,("custom-key","custom-value")
        ]

----------------------------------------------------------------

e411 :: HeaderBlock
e411 = [
    Literal Add (Idx 8) "302"
  , Literal Add (Idx 24) "private"
  , Literal Add (Idx 34) "Mon, 21 Oct 2013 20:13:21 GMT"
  , Literal Add (Idx 48) "https://www.example.com"
  ]

e411h :: HeaderSet
e411h = [(":status","302")
        ,("cache-control","private")
        ,("date","Mon, 21 Oct 2013 20:13:21 GMT")
        ,("location","https://www.example.com")
        ]

e412 :: HeaderBlock
e412 = [
    Indexed 4
  , Indexed 12
  ]

e412h :: HeaderSet
e412h = [(":status","200")
        ,("cache-control","private")
        ,("date","Mon, 21 Oct 2013 20:13:21 GMT")
        ,("location","https://www.example.com")
        ]

e413 :: HeaderBlock
e413 = [
    Indexed 3
  , Indexed 4
  , Indexed 4
  , Literal Add (Idx 3) "Mon, 21 Oct 2013 20:13:22 GMT"
  , Literal Add (Idx 29) "gzip"
  , Indexed 4
  , Indexed 4
  , Indexed 3
  , Indexed 3
  , Literal Add (Idx 58) "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"
  ]

e413h :: HeaderSet
e413h = [("cache-control","private")
        ,("date","Mon, 21 Oct 2013 20:13:22 GMT")
        ,("content-encoding","gzip")
        ,("location","https://www.example.com")
        ,(":status","200")
        ,("set-cookie","foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1")]
