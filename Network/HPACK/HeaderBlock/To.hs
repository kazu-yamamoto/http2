{-# LANGUAGE BangPatterns #-}

module Network.HPACK.HeaderBlock.To (
    toHeaderBlock
  ) where

import Control.Applicative ((<$>))
import Network.HPACK.Builder
import Network.HPACK.Context
import Network.HPACK.HeaderBlock.HeaderField
import Network.HPACK.Table

type Ctx = (Context, Builder HeaderField)

-- | Encoding 'HeaderSet' to 'HeaderBlock'.
toHeaderBlock :: Context
              -> HeaderSet
              -> IO (Context, HeaderBlock)
toHeaderBlock !ctx hs = encodeInit ctx >>= toHeaderBlock' hs

toHeaderBlock' :: HeaderSet
               -> Ctx
               -> IO (Context, HeaderBlock)
toHeaderBlock' (h:hs) !ctx = encodeStep ctx h >>= toHeaderBlock' hs
toHeaderBlock' []     !ctx = encodeFinal ctx

----------------------------------------------------------------
-- A simple encoding strategy to reset the reference set first
-- by 'Index 0' and uses indexing as much as possible.

encodeStep :: Ctx -> Header -> IO Ctx
encodeStep (!ctx,!builder) h@(k,v) = do
    cache <- lookupHeader h ctx
    let e = toEntry h
    case cache of
        None -> do
            let builder' = builder << Literal Add (Lit k) v
            ctx' <- newEntry ctx e
            return (ctx', builder')
        KeyOnly InStaticTable i  -> do
            let builder' = builder << Literal Add (Idx i) v
            ctx' <- newEntry ctx e
            return (ctx', builder')
        KeyOnly InHeaderTable i  -> do
            let builder' = builder << Literal Add (Idx i) v
            ctx' <- newEntry ctx e
            return (ctx', builder')
        KeyValue InStaticTable i -> do
            let builder' = builder << Indexed i
            ctx' <- newEntry ctx e
            return (ctx', builder')
        KeyValue InHeaderTable i -> do
            (builder',ctx') <- if i `isPresentIn` ctx then do
                  let b = builder << Indexed i << Indexed i
                      c = ctx
                  return (b,c)
                else do
                  let b = builder << Indexed i
                  c <- pushRef ctx i e
                  return (b,c)
            return (ctx', builder')

encodeInit :: Context -> IO Ctx
encodeInit ctx = do
    ctx' <- clearHeaderSet <$> clearRefSets ctx
    let initialHeaderBlock = singleton $ Indexed 0
    return (ctx', initialHeaderBlock)

encodeFinal :: Ctx -> IO (Context, HeaderBlock)
encodeFinal (ctx, builder) = do
    !ctx' <- emitNotEmitted ctx
    let !hb = run builder
    return (ctx', hb)
