{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Network.HPACK.HeaderBlock.To (
    toHeaderBlock
  ) where

import Network.HPACK.Builder
import Network.HPACK.Context
import Network.HPACK.HeaderBlock.HeaderField
import Network.HPACK.Table
import Network.HPACK.Types

type Ctx = (Context, Builder HeaderField)
type Step = Ctx -> Header -> IO Ctx

-- | Encoding 'HeaderList' to 'HeaderBlock'.
toHeaderBlock :: CompressionAlgo
              -> Context
              -> HeaderList
              -> IO (Context, HeaderBlock)
toHeaderBlock Naive  !ctx hs = encodeLoop naiveStep  hs (ctx,empty)
toHeaderBlock Static !ctx hs = encodeLoop staticStep hs (ctx,empty)
toHeaderBlock Linear !ctx hs = encodeLoop linearStep hs (ctx,empty)

----------------------------------------------------------------

encodeFinal :: Ctx -> IO (Context, HeaderBlock)
encodeFinal (!ctx, !builder) = return (ctx, run builder)

encodeLoop :: Step
           -> HeaderList
           -> Ctx
           -> IO (Context, HeaderBlock)
encodeLoop step (h:hs) !ctx = step ctx h >>= encodeLoop step hs
encodeLoop _    []     !ctx = encodeFinal ctx

----------------------------------------------------------------

naiveStep :: Step
naiveStep (!ctx,!builder) (k,v) = do
    let builder' = builder << Literal NotAdd (Lit k) v
    return (ctx, builder')

----------------------------------------------------------------

staticStep :: Step
staticStep (!ctx,!builder) h@(k,v) = return (ctx, builder')
  where
    b = case lookupHeader h ctx of
        None                     -> Literal NotAdd (Lit k) v
        KeyOnly  InStaticTable i -> Literal NotAdd (Idx i) v
        KeyOnly  InHeaderTable _ -> Literal NotAdd (Lit k) v
        KeyValue InStaticTable i -> Literal NotAdd (Idx i) v
        KeyValue InHeaderTable _ -> Literal NotAdd (Lit k) v
    builder' = builder << b

----------------------------------------------------------------
-- A simple encoding strategy to reset the reference set first
-- by 'Index 0' and uses indexing as much as possible.

linearStep :: Step
linearStep cb@(!ctx,!builder) h = smartStep linear cb h
  where
    linear i = return (ctx,builder << Indexed i)

----------------------------------------------------------------

smartStep :: (Index -> IO Ctx) -> Step
smartStep func cb@(!ctx,!builder) h@(k,_) = do
    let cache = lookupHeader h ctx
    case cache of
        None                     -> check cb h (Lit k)
        KeyOnly  InStaticTable i -> check cb h (Idx i)
        KeyOnly  InHeaderTable i -> check cb h (Idx i)
        KeyValue InStaticTable i -> return (ctx, builder << Indexed i)
        KeyValue InHeaderTable i -> func i

check :: Ctx -> Header -> Naming -> IO Ctx
check (ctx,builder) h@(k,v) x
  | k `elem` headersNotToIndex = do
      let builder' = builder << Literal NotAdd x v
      return (ctx, builder')
  | otherwise = do
      let e = toEntry h
      ctx' <- newEntryForEncoding ctx e
      let builder' = builder << Literal Add x v
      return (ctx', builder')

headersNotToIndex :: [HeaderName]
headersNotToIndex = [
    ":path"
  , "content-length"
  , "location"
  , "etag"
  , "set-cookie"
  ]
