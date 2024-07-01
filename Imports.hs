module Imports (
    ByteString (..),
    ShortByteString,
    module Control.Applicative,
    module Control.Monad,
    module Data.Bits,
    module Data.Either,
    module Data.List,
    module Data.Foldable,
    module Data.Int,
    module Data.Maybe,
    module Data.Monoid,
    module Data.Ord,
    module Data.String,
    module Data.Word,
    module Numeric,
    module Network.HTTP.Semantics,
    module Network.HTTP.Types,
    module Data.CaseInsensitive,
    GCBuffer,
    withForeignPtr,
    mallocPlainForeignPtrBytes,
    labelMe,
) where

import Control.Applicative
import Control.Monad
import Data.Bits hiding (Bits)
import Data.ByteString.Internal (ByteString (..))
import Data.ByteString.Short (ShortByteString)
import Data.CaseInsensitive (foldedCase, mk, original)
import Data.Either
import Data.Foldable
import Data.Int
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.String
import Data.Word
import Foreign.ForeignPtr
import GHC.Conc.Sync
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import Network.HTTP.Semantics
import Network.HTTP.Types
import Numeric

type GCBuffer = ForeignPtr Word8

labelMe :: String -> IO ()
labelMe l = do
    tid <- myThreadId
    labelThread tid l
