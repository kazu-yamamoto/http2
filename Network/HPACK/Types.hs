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

-- Size is len of name + len of value + 32
type Entry = (Size,Header)

type Table = Array Index Entry

type Size = Int

data StaticTable = StaticTable Size Table
data HeaderTable = HeaderTable {
    maxNumOfEntries :: Int
  , offset :: Index
  , numOfEntries :: Int
  , circularTable :: Table
  }

data ReferenceSet = ReferenceSet [Index]

data Context = Context {
    headerTable :: HeaderTable
  , oldReferenceSet :: ReferenceSet -- not emitted
  , newReferenceSet :: ReferenceSet -- emitted
  , headerSet :: HeaderSet
  }

data DecodeError = IndexOverrun

data WhichTable = InHeaderTable Entry
                | InStaticTable Entry
                | IndexError
                deriving Eq
