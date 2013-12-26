{-# LANGUAGE BangPatterns #-}

module Network.HPACK.HeaderBlock.To (
    toHeaderBlock
  ) where

import Control.Applicative ((<$>))
import Network.HPACK.Context
import Network.HPACK.HeaderBlock.HeaderField
import Network.HPACK.Table

-- | Encoding 'HeaderSet' to 'HeaderBlock'.
toHeaderBlock :: HeaderSet
              -> Context
              -> IO (HeaderBlock, Context)
toHeaderBlock hs !ctx = encodeInit ctx >>= toHeaderBlock' hs

toHeaderBlock' :: HeaderSet
               -> (DL, Context)
               -> IO (HeaderBlock, Context)
toHeaderBlock' (h:hs) !ctx = encodeStep ctx h >>= toHeaderBlock' hs
toHeaderBlock' []     !ctx = encodeFinal ctx

----------------------------------------------------------------
-- A simple encoding strategy to reset the reference set first
-- by 'Index 0' and uses indexing as much as possible.

encodeStep :: (DL,Context) -> Header -> IO (DL,Context)
encodeStep (!dl,!ctx) h@(k,v) = do
    cache <- lookupHeader h ctx
    let e = toEntry h
    case cache of
        None -> do
            let dl' = dl << Literal Add (Lit k) v
            ctx' <- newEntry ctx e
            return (dl', ctx')
        KeyOnly InStaticTable i  -> do
            let dl' = dl << Literal Add (Idx i) v
            ctx' <- newEntry ctx e
            return (dl', ctx')
        KeyOnly InHeaderTable i  -> do
            let dl' = dl << Literal Add (Idx i) v
            ctx' <- newEntry ctx e
            return (dl', ctx')
        KeyValue InStaticTable i -> do
            let dl' = dl << Indexed i
            ctx' <- newEntry ctx e
            return (dl', ctx')
        KeyValue InHeaderTable i -> do
            (dl',ctx') <- if i `isPresentIn` ctx then do
                  let d = dl << Indexed i << Indexed i
                      c = ctx
                  return (d,c)
                else do
                  let d = dl << Indexed i
                  c <- pushRef ctx i e
                  return (d,c)
            return (dl', ctx')

encodeInit :: Context -> IO (DL, Context)
encodeInit ctx = do
    ctx' <- clearHeaderSet <$> clearRefSets ctx
    let initialHeaderBlock = initResult $ Indexed 0
    return (initialHeaderBlock, ctx')

encodeFinal :: (DL, Context) -> IO (HeaderBlock, Context)
encodeFinal (dl,ctx) = do
    !ctx' <- emitNotEmitted ctx
    let !hb = getResult dl
    return (hb, ctx')

----------------------------------------------------------------

type DL = HeaderBlock -> HeaderBlock

(<<) :: DL -> HeaderField -> DL
dl << entry = dl . (entry :)

initResult :: HeaderField -> DL
initResult hf = (hf :)

getResult :: DL -> HeaderBlock
getResult dl = dl []
