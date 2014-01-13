{-# LANGUAGE OverloadedStrings #-}

module HeaderBlock where

import Network.HPACK.Context
import Network.HPACK.HeaderBlock
import Network.HPACK.Types

import HexString

----------------------------------------------------------------

e31 :: HeaderBlock
e31 = [
    Indexed 2
  , Indexed 7
  , Indexed 6
  , Literal Add (Idx 4) "www.example.com"
  ]

e31h :: HeaderSet
e31h = [(":method","GET")
        ,(":scheme","http")
        ,(":path","/")
        ,(":authority","www.example.com")
        ]

e31b :: ByteStream
e31b = fromHexString "828786048bdb6d883e68d1cb1225ba7f"

e32 :: HeaderBlock
e32 = [
    Literal Add (Idx 27) "no-cache"
  ]

e32h :: HeaderSet
e32h = [("cache-control","no-cache")
        ,(":method","GET")
        ,(":scheme","http")
        ,(":path","/")
        ,(":authority","www.example.com")]

e32b :: ByteStream
e32b = fromHexString "1b8663654a1398ff"

e33 :: HeaderBlock
e33 = [
    Indexed 0
  , Indexed 5
  , Indexed 12
  , Indexed 11
  , Indexed 4
  , Literal Add (Lit "custom-key") "custom-value"
  ]

e33h :: HeaderSet
e33h = [(":method","GET")
        ,(":scheme","https")
        ,(":path","/index.html")
        ,(":authority","www.example.com")
        ,("custom-key","custom-value")
        ]

e33b :: ByteStream
e33b = fromHexString "80858c8b8400884eb08b749790fa7f894eb08b74979a17a8ff"

----------------------------------------------------------------

e51 :: HeaderBlock
e51 = [
    Literal Add (Idx 8) "302"
  , Literal Add (Idx 24) "private"
  , Literal Add (Idx 34) "Mon, 21 Oct 2013 20:13:21 GMT"
  , Literal Add (Idx 48) "https://www.example.com"
  ]

e51h :: HeaderSet
e51h = [(":status","302")
        ,("cache-control","private")
        ,("date","Mon, 21 Oct 2013 20:13:21 GMT")
        ,("location","https://www.example.com")
        ]

e51b :: ByteStream
e51b = fromHexString "0882409f1886c31b39bf387f2292a2fba20320f2ab303124018b490d3209e8773093e39e7864dd7afd3d3d248747db87284955f6ff"

e52 :: HeaderBlock
e52 = [
    Indexed 4
  , Indexed 12
  ]

e52h :: HeaderSet
e52h = [(":status","200")
        ,("cache-control","private")
        ,("date","Mon, 21 Oct 2013 20:13:21 GMT")
        ,("location","https://www.example.com")
        ]

e52b :: ByteStream
e52b = fromHexString "848c"

e53 :: HeaderBlock
e53 = [
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

e53h :: HeaderSet
e53h = [("cache-control","private")
        ,("date","Mon, 21 Oct 2013 20:13:22 GMT")
        ,("content-encoding","gzip")
        ,("location","https://www.example.com")
        ,(":status","200")
        ,("set-cookie","foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1")]

e53b :: ByteStream
e53b = fromHexString "8384840392a2fba20320f2ab303124018b490d3309e8771d84e1fbb30f848483833ab3df7dfb36d3d9e1fcfc3fafe7abfcfefcbfaf3edf2f977fd36ff7fd79f6f977fd3de16bfa46fe10d889447de1ce18e565f76c2f"
