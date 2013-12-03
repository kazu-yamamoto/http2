module Network.HPACK.Decode (
    DecodeError(..)
  , decode
  , decodeStep
  ) where

import Data.List (partition)
import Network.HPACK.Context
import Network.HPACK.Entry
import Network.HPACK.HeaderTable
import Network.HPACK.ReferenceSet
import Network.HPACK.Types

----------------------------------------------------------------

data DecodeError = IndexOverrun deriving Show

----------------------------------------------------------------

decode :: Context -> HeaderBlock -> Either DecodeError Context
decode ctx (r:rs) = case decodeStep ctx r of
    Left  err  -> Left err
    Right ctx' -> decode ctx' rs
decode ctx [] = case allEntries oldref hdrtbl of
    Left  e          -> Left e
    Right notEmitted -> Right $ emit ctx notEmitted
  where
    oldref = oldReferenceSet ctx
    hdrtbl = headerTable ctx

emit :: Context -> HeaderSet -> Context
emit (Context hdrtbl oldref newref hdrset) notEmitted = ctx
  where
    hdrset' = reverse $ notEmitted ++ hdrset
    oldref' = mergeReferenceSet newref oldref
    ctx = Context hdrtbl oldref' emptyReferenceSet hdrset'

----------------------------------------------------------------

decodeStep :: Context -> Representation -> Either DecodeError Context
decodeStep ctx (Indexed idx)
  | idx == 0  = Right $ ctx { oldReferenceSet = emptyReferenceSet
                            , newReferenceSet = emptyReferenceSet }
  | present   = Right $ ctx { oldReferenceSet = removeIndex idx oldref }
  | otherwise = decodeNotPresent ctx idx (hdrtbl .!. idx)
  where
    oldref = oldReferenceSet ctx
    hdrtbl = headerTable ctx
    present = idx `isPresent` oldref
decodeStep ctx (Literal NotAdd naming v) = case fromNaming naming hdrtbl of
    Right k -> Right $ emitOnly (k,v) ctx
    Left  e -> Left e
  where
    hdrtbl = headerTable ctx
decodeStep ctx (Literal Add naming v) = case fromNaming naming hdrtbl of
    Right k -> Right $ newEntry (toEntry (k,v)) ctx
    Left  e -> Left e
  where
    hdrtbl = headerTable ctx

----------------------------------------------------------------

decodeNotPresent :: Context -> Index -> WhichTable -> Either DecodeError Context
decodeNotPresent _   _   IndexError        = Left IndexOverrun
decodeNotPresent ctx _   (InStaticTable e) = Right $ newEntry e ctx
decodeNotPresent ctx idx (InHeaderTable e) = Right $ pushRef idx e ctx


----------------------------------------------------------------

fromNaming :: Naming -> HeaderTable -> Either DecodeError HeaderName
fromNaming (Lit k)   _  = Right k
fromNaming (Idx idx) hdrtbl = case hdrtbl .!. idx of
    InHeaderTable e -> Right $ entryHeaderName e
    InStaticTable e -> Right $ entryHeaderName e
    IndexError      -> Left IndexOverrun

----------------------------------------------------------------

allEntries :: ReferenceSet -> HeaderTable -> Either DecodeError HeaderSet
allEntries (ReferenceSet is) hdrtbl
  | null ls   = Right xs
  | otherwise = Left IndexOverrun
  where
    ws = map (\i -> hdrtbl .!. i) is
    (ls,rs) = partition (== IndexError) ws
    fromWhich (InHeaderTable e) = e
    fromWhich (InStaticTable e) = e
    fromWhich _                 = error "fromWhich"
    xs = map (fromEntry . fromWhich) rs
