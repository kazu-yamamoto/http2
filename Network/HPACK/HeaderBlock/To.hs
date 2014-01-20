{-# LANGUAGE BangPatterns #-}

module Network.HPACK.HeaderBlock.To (
    toHeaderBlock
  ) where

import Data.List (foldl')
import Network.HPACK.Builder
import Network.HPACK.Context
import Network.HPACK.HeaderBlock.HeaderField
import Network.HPACK.Table
import Network.HPACK.Types

type Ctx = (Context, Builder HeaderField)
type Step = Ctx -> Header -> IO Ctx

-- | Encoding 'HeaderSet' to 'HeaderBlock'.
toHeaderBlock :: CompressionAlgo
              -> Context
              -> HeaderSet
              -> IO (Context, HeaderBlock)
toHeaderBlock Naive  !ctx hs = reset ctx >>= encodeLoop naiveStep  hs
toHeaderBlock Linear !ctx hs = reset ctx >>= encodeLoop linearStep hs
toHeaderBlock Diff   !ctx hs = encodeLoop diffStep hs (ctx, empty)

----------------------------------------------------------------

encodeFinal :: Ctx -> IO (Context, HeaderBlock)
encodeFinal (!ctx, !builder) = do
    (is,!ctx') <- emitNotEmittedForEncoding ctx
    let builder' = foldl' (\b i -> b << Indexed i) builder is
        !hb = run builder'
    return (ctx', hb)

encodeLoop :: Step
           -> HeaderSet
           -> Ctx
           -> IO (Context, HeaderBlock)
encodeLoop step (h:hs) !ctx = step ctx h >>= encodeLoop step hs
encodeLoop _    []     !ctx = encodeFinal ctx

----------------------------------------------------------------

-- for naiveStep and linearStep
reset :: Context -> IO Ctx
reset ctx = do
    let ctx' = clearRefSets ctx
        initialHeaderBlock = singleton $ Indexed 0
    return (ctx', initialHeaderBlock)

----------------------------------------------------------------

naiveStep :: Step
naiveStep (!ctx,!builder) (k,v) = do
    let builder' = builder << Literal NotAdd (Lit k) v
    return (ctx, builder')

----------------------------------------------------------------
-- A simple encoding strategy to reset the reference set first
-- by 'Index 0' and uses indexing as much as possible.

linearStep :: Step
linearStep cb@(!ctx,!builder) h = smartStep linear cb h
  where
    linear i
      | i `isPresentIn` ctx = do
          let b = builder << Indexed i << Indexed i
          return (ctx,b)
      | otherwise = do
          let b = builder << Indexed i
              c = pushRef ctx i
          return (c,b)

----------------------------------------------------------------
-- See http://lists.w3.org/Archives/Public/ietf-http-wg/2013JulSep/1135.html

diffStep :: Step
diffStep cb@(!ctx,!builder) h = smartStep diff cb h
  where
    diff i = case checkAndUpdate i ctx of
        (Z,  ctx') -> do
            let b = builder << Indexed i
                c = pushRef ctx' i
            return (c,b)
        (E0, ctx') -> return (ctx', builder)
        (E2, ctx') -> do
            let b = builder << Indexed i << Indexed i
            return (ctx',b)
        (E4, ctx') -> do
            let b = builder << Indexed i << Indexed i << Indexed i << Indexed i
            return (ctx',b)

----------------------------------------------------------------

smartStep :: (Index -> IO Ctx) -> Step
smartStep func (!ctx,!builder) h@(k,v) = do
    cache <- lookupHeader h ctx
    let e = toEntry h
    case cache of
        None -> do
            (is,ctx') <- newEntryForEncoding ctx e
            let builder' = double is builder << Literal Add (Lit k) v
            return (ctx', builder')
        KeyOnly InStaticTable i  -> do
            (is,ctx') <- newEntryForEncoding ctx e
            let builder' = double is builder << Literal Add (Idx i) v
            return (ctx', builder')
        KeyOnly InHeaderTable i  -> do
            (is,ctx') <- newEntryForEncoding ctx e
            let builder' = double is builder << Literal Add (Idx i) v
            return (ctx', builder')
        KeyValue InStaticTable i -> do
            (is,ctx') <- newEntryForEncoding ctx e
            let builder' = double is builder << Indexed i
            return (ctx', builder')
        KeyValue InHeaderTable i -> func i

double :: [Index] -> Builder HeaderField -> Builder HeaderField
double []     b = b
double (i:is) b = double is (b << Indexed i << Indexed i)
