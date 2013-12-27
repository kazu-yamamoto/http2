{-# LANGUAGE BangPatterns #-}

module Network.HPACK.HeaderBlock.To (
    toHeaderBlock
  ) where

import Control.Applicative ((<$>))
import Network.HPACK.Context
import Network.HPACK.HeaderBlock.HeaderField
import Network.HPACK.Table

-- | Encoding 'HeaderSet' to 'HeaderBlock'.
toHeaderBlock :: Context
              -> HeaderSet
              -> IO (Context, HeaderBlock)
toHeaderBlock !ctx hs = encodeInit ctx >>= toHeaderBlock' hs

toHeaderBlock' :: HeaderSet
               -> (Context, DL)
               -> IO (Context, HeaderBlock)
toHeaderBlock' (h:hs) !ctx = encodeStep ctx h >>= toHeaderBlock' hs
toHeaderBlock' []     !ctx = encodeFinal ctx

----------------------------------------------------------------
-- A simple encoding strategy to reset the reference set first
-- by 'Index 0' and uses indexing as much as possible.

encodeStep :: (Context, DL) -> Header -> IO (Context, DL)
encodeStep (!ctx,!dl) h@(k,v) = do
    cache <- lookupHeader h ctx
    let e = toEntry h
    case cache of
        None -> do
            let dl' = dl << Literal Add (Lit k) v
            ctx' <- newEntry ctx e
            return (ctx', dl')
        KeyOnly InStaticTable i  -> do
            let dl' = dl << Literal Add (Idx i) v
            ctx' <- newEntry ctx e
            return (ctx', dl')
        KeyOnly InHeaderTable i  -> do
            let dl' = dl << Literal Add (Idx i) v
            ctx' <- newEntry ctx e
            return (ctx', dl')
        KeyValue InStaticTable i -> do
            let dl' = dl << Indexed i
            ctx' <- newEntry ctx e
            return (ctx', dl')
        KeyValue InHeaderTable i -> do
            (dl',ctx') <- if i `isPresentIn` ctx then do
                  let d = dl << Indexed i << Indexed i
                      c = ctx
                  return (d,c)
                else do
                  let d = dl << Indexed i
                  c <- pushRef ctx i e
                  return (d,c)
            return (ctx', dl')

encodeInit :: Context -> IO (Context, DL)
encodeInit ctx = do
    ctx' <- clearHeaderSet <$> clearRefSets ctx
    let initialHeaderBlock = initResult $ Indexed 0
    return (ctx', initialHeaderBlock)

encodeFinal :: (Context, DL) -> IO (Context, HeaderBlock)
encodeFinal (ctx, dl) = do
    !ctx' <- emitNotEmitted ctx
    let !hb = getResult dl
    return (ctx', hb)

----------------------------------------------------------------

-- FIXME
type DL = HeaderBlock -> HeaderBlock

(<<) :: DL -> HeaderField -> DL
dl << entry = dl . (entry :)

initResult :: HeaderField -> DL
initResult hf = (hf :)

getResult :: DL -> HeaderBlock
getResult dl = dl []
