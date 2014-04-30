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
d41b = fromHexString "828786448ce7cf9bebe89b6fb16fa9b6ff"

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
d42b = fromHexString "5c86b9b9949556bf"

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
d43b = fromHexString "30858c8b844088571c5cdb737b2faf89571c5cdb73724d9c57"

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
d61b = fromHexString "488240175985bf06724b976393d6dbb29884de2a718805062098513109b56ba37191adcebf198e7e7cf9bebe89b6fb16fa9b6f"

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
d63b = fromHexString "84844393d6dbb29884de2a718805062098513111b56ba35e84abdd97ff848483837bb1e0d6cf9f6e8f9fd3e5f6fa76fefd3c7edf9eff1f2f0f3cfe9f6fcf7f8f879f61ad4f4cc9a973a2200ec3725e18b1b74e3f"
