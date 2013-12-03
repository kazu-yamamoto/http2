module Network.HPACK.Types (
  -- * Header
    HeaderName
  , HeaderValue
  , Header
  , HeaderSet
  -- * Representation
  , Index
  , Indexing(..)
  , Naming(..)
  , Representation(..)
  , HeaderBlock
  -- * Table
  , Size
  , Entry
  , Table
  ) where

import Data.Array (Array)
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

data Representation = Indexed Index
                    | Literal Indexing Naming HeaderValue
                    deriving Show

type HeaderBlock = [Representation]

----------------------------------------------------------------

type Size = Int

-- Size is len of name + len of value + 32
type Entry = (Size,Header)

type Table = Array Index Entry
