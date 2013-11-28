module Network.HPACK.Types where

import Data.ByteString (ByteString)
import Data.Array

type Index = Int
type HeaderName = ByteString
type HeaderValue = ByteString
type Header = (HeaderName, HeaderValue)

type HeaderSet = [Header]

data Indexing = Add | NotAdd

data Naming = Idx Index | Lit HeaderName

type HeaderBlock = [Representation]

data Representation = Indexed Index
                    | Literal Indexing Naming HeaderValue

type Table = Array Int Header

type Size = Int

data StaticTable = StaticTable Size Table
data HeaderTable = HeaderTable Size  -- Table size
                               Index -- Offset
                               Int   -- # of entries
                               Table

data ReferenceSet = ReferenceSet [Index]

data Context = Context {
    headerTable :: HeaderTable
  , referenceSet :: ReferenceSet
  , headerSet :: HeaderSet
  }

data DecodeError = IndexOverrun

data WhichTable = InHeaderTable Header
                | InStaticTable Header
                | IndexError
