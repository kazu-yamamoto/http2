module Network.HPACK.Types where

import Data.ByteString (ByteString)
import Data.Array

type Index = Int
type HeaderName = ByteString
type HeaderValue = ByteString

type HeaderSet = [(HeaderName, HeaderValue)]

data Indexing = Add | NotAdd

data Naming = Idx Index | Lit HeaderName

type HeaderBlock = [Representation]

data Representation = Indexed Index
                    | Literal Indexing Naming HeaderValue

type Table = Array Int (HeaderName, HeaderValue)

type Size = Int

data StaticTable = StaticTable Size Table
data HeaderTable = HeaderTable Size Index Index Table

data ReferenceSet = ReferenceSet [Index]

data Context = Context {
    headerTable :: HeaderTable
  , referenceSet :: ReferenceSet
  , headerSet :: HeaderSet
  }

data DecodeError = IndexOverrun

data WhichTable = InHeaderTable HeaderName HeaderValue
                | InStaticTable HeaderName HeaderValue
                | IndexError

