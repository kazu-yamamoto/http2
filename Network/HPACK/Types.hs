module Network.HPACK.Types where

import Data.ByteString (ByteString)

----------------------------------------------------------------

type HeaderName = ByteString
type HeaderValue = ByteString
type Header = (HeaderName, HeaderValue)
type HeaderSet = [Header]

----------------------------------------------------------------

type Index = Int

data Indexing = Add | NotAdd deriving Show

data Naming = Idx Index | Lit HeaderName deriving Show

type HeaderBlock = [Representation]

data Representation = Indexed Index
                    | Literal Indexing Naming HeaderValue
                    deriving Show

----------------------------------------------------------------

type Size = Int
