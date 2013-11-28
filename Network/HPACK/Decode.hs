module Network.HPACK.Decode where

import Network.HPACK.Types
import Network.HPACK.Context

----------------------------------------------------------------

decodeStep :: Context -> Representation -> Either DecodeError Context
decodeStep ctx (Indexed idx)
  | idx == 0               = Right $ ctx { referenceSet = emptyReferenceSet }
  | idx `isPresent` refset = Right $ ctx { referenceSet = removeIndex idx refset }
  | otherwise              = decodeNotPresent ctx $ magicalIndex idx hdrtbl
  where
    refset = referenceSet ctx
    hdrtbl = headerTable ctx

decodeStep ctx (Literal NotAdd naming v) = case fromNaming naming hdrtbl of
    Right k -> Right $ updateHeaderSet ctx k v
    Left  e -> Left e
  where
    hdrtbl = headerTable ctx
decodeStep ctx (Literal Add naming v) = case fromNaming naming hdrtbl of
    Right k -> Right $ updateHeaderTable ctx k v
    Left e  -> Left e
  where
    hdrtbl = headerTable ctx

----------------------------------------------------------------

decodeNotPresent :: Context -> WhichTable -> Either DecodeError Context
decodeNotPresent _   IndexError          = Left IndexOverrun
decodeNotPresent ctx (InHeaderTable k v) = Right $ updateHeaderTable ctx k v
decodeNotPresent ctx (InStaticTable k v) = Right ctx'
  where
    refset = referenceSet ctx
    hdrtbl = headerTable ctx
    hdrset = headerSet ctx
    hdrset' = (k,v) : hdrset
    refset' = addIndex 1 refset -- FIXME
    ctx' = Context hdrtbl refset' hdrset'

----------------------------------------------------------------

updateHeaderTable :: Context -> HeaderName -> HeaderValue -> Context
updateHeaderTable ctx k v = ctx'
  where
    refset = referenceSet ctx
    hdrtbl = headerTable ctx
    hdrset = headerSet ctx
    hdrset' = (k,v) : hdrset
    (hdrtbl', refset') = insertEntry k v hdrtbl refset
    refset'' = addIndex 1 refset' -- FIXME
    ctx' = Context hdrtbl' refset'' hdrset'

updateHeaderSet :: Context -> HeaderName -> HeaderValue -> Context
updateHeaderSet ctx k v = ctx'
  where
    hdrset = headerSet ctx
    hdrset' = (k,v) : hdrset
    ctx' = ctx { headerSet = hdrset' }
