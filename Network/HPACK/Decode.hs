module Network.HPACK.Decode (
    DecodeError(..)
  , decode
  , decodeStep
  ) where

import Network.HPACK.Context
import Network.HPACK.Entry
import Network.HPACK.HeaderTable
import Network.HPACK.Types

----------------------------------------------------------------

data DecodeError = IndexOverrun deriving Show

----------------------------------------------------------------

decode :: Context -> HeaderBlock -> Either DecodeError Context
decode ctx (r:rs) = case decodeStep ctx r of
    Left  err  -> Left err
    Right ctx' -> decode ctx' rs
decode ctx [] = case allEntries ctx of
    Left  e          -> Left e
    Right notEmitted -> Right $ emit ctx notEmitted

----------------------------------------------------------------

decodeStep :: Context -> Representation -> Either DecodeError Context
decodeStep ctx (Indexed idx)
  | idx == 0  = Right $ emptyRefSets ctx
  | present   = Right $ removeRef idx ctx
  | otherwise = decodeNotPresent ctx idx $ whichTable idx ctx
  where
    present = doesExist idx ctx
decodeStep ctx (Literal NotAdd naming v) = case fromNaming naming ctx of
    Right k -> Right $ emitOnly (k,v) ctx
    Left  e -> Left e
decodeStep ctx (Literal Add naming v) = case fromNaming naming ctx of
    Right k -> Right $ newEntry (toEntry (k,v)) ctx
    Left  e -> Left e

----------------------------------------------------------------

decodeNotPresent :: Context -> Index -> WhichTable -> Either DecodeError Context
decodeNotPresent _   _   IndexError        = Left IndexOverrun
decodeNotPresent ctx _   (InStaticTable e) = Right $ newEntry e ctx
decodeNotPresent ctx idx (InHeaderTable e) = Right $ pushRef idx e ctx

----------------------------------------------------------------

fromNaming :: Naming -> Context -> Either DecodeError HeaderName
fromNaming (Lit k)   _  = Right k
fromNaming (Idx idx) ctx = case whichTable idx ctx of
    InHeaderTable e -> Right $ entryHeaderName e
    InStaticTable e -> Right $ entryHeaderName e
    IndexError      -> Left IndexOverrun

----------------------------------------------------------------

allEntries :: Context -> Either DecodeError HeaderSet
allEntries ctx
  | null ls   = Right xs
  | otherwise = Left IndexOverrun
  where
    (ls,rs) = allEnts ctx
    xs = map (fromEntry . fromWhich) rs
    fromWhich (InHeaderTable e) = e
    fromWhich (InStaticTable e) = e
    fromWhich _                 = error "fromWhich"
