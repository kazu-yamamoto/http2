module Network.HPACK.Types where

import Data.ByteString (ByteString)
import Data.Array

type Index = Int
type HeaderName = ByteString
type HeaderValue = ByteString
type Header = (HeaderName, HeaderValue)

type HeaderSet = [Header]

data Indexing = Add | NotAdd deriving Show

data Naming = Idx Index | Lit HeaderName deriving Show

type HeaderBlock = [Representation]

data Representation = Indexed Index
                    | Literal Indexing Naming HeaderValue
                    deriving Show

-- Size is len of name + len of value + 32
type Entry = (Size,Header)

type Table = Array Index Entry

type Size = Int

data StaticTable = StaticTable Size Table deriving Show

data HeaderTable = HeaderTable {
    maxNumOfEntries :: Int
  , offset :: Index
  , numOfEntries :: Int
  , circularTable :: Table
  , headerTableSize :: Size
  , maxHeaderTableSize :: Size
  } deriving Show

data ReferenceSet = ReferenceSet [Index] deriving Show

data Context = Context {
    headerTable :: HeaderTable
  , oldReferenceSet :: ReferenceSet -- not emitted
  , newReferenceSet :: ReferenceSet -- emitted
  , headerSet :: HeaderSet
  } deriving Show

data DecodeError = IndexOverrun deriving Show

data WhichTable = InHeaderTable Entry
                | InStaticTable Entry
                | IndexError
                deriving Eq
