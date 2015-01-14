{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Network.HPACK.HeaderBlock.To (
    toHeaderBlock
  ) where

import Network.HPACK.Builder
import Network.HPACK.HeaderBlock.HeaderField
import Network.HPACK.Table
import Network.HPACK.Types

type Ctx = (DynamicTable, Builder HeaderField)
type Step = Ctx -> Header -> IO Ctx

-- | Encoding 'HeaderList' to 'HeaderBlock'.
toHeaderBlock :: CompressionAlgo
              -> DynamicTable
              -> HeaderList
              -> IO (DynamicTable, HeaderBlock)
toHeaderBlock Naive  !hdrtbl hs = encodeLoop naiveStep  hs (hdrtbl,empty)
toHeaderBlock Static !hdrtbl hs = encodeLoop staticStep hs (hdrtbl,empty)
toHeaderBlock Linear !hdrtbl hs = encodeLoop linearStep hs (hdrtbl,empty)

----------------------------------------------------------------

encodeFinal :: Ctx -> IO (DynamicTable, HeaderBlock)
encodeFinal (!hdrtbl, !builder) = return (hdrtbl, run builder)

encodeLoop :: Step
           -> HeaderList
           -> Ctx
           -> IO (DynamicTable, HeaderBlock)
encodeLoop step (h:hs) !hdrtbl = step hdrtbl h >>= encodeLoop step hs
encodeLoop _    []     !hdrtbl = encodeFinal hdrtbl

----------------------------------------------------------------

naiveStep :: Step
naiveStep (!hdrtbl,!builder) (k,v) = do
    let builder' = builder << Literal NotAdd (Lit k) v
    return (hdrtbl, builder')

----------------------------------------------------------------

staticStep :: Step
staticStep (!hdrtbl,!builder) h@(k,v) = return (hdrtbl, builder')
  where
    b = case lookupTable h hdrtbl of
        None                     -> Literal NotAdd (Lit k) v
        KeyOnly  InStaticTable i -> Literal NotAdd (Idx i) v
        KeyOnly  InDynamicTable _ -> Literal NotAdd (Lit k) v
        KeyValue InStaticTable i -> Literal NotAdd (Idx i) v
        KeyValue InDynamicTable _ -> Literal NotAdd (Lit k) v
    builder' = builder << b

----------------------------------------------------------------
-- A simple encoding strategy to reset the reference set first
-- by 'Index 0' and uses indexing as much as possible.

linearStep :: Step
linearStep cb@(!hdrtbl,!builder) h = smartStep linear cb h
  where
    linear i = return (hdrtbl,builder << Indexed i)

----------------------------------------------------------------

smartStep :: (Index -> IO Ctx) -> Step
smartStep func cb@(!hdrtbl,!builder) h@(k,_) = do
    let cache = lookupTable h hdrtbl
    case cache of
        None                      -> check cb h (Lit k)
        KeyOnly  InStaticTable i  -> check cb h (Idx i)
        KeyOnly  InDynamicTable i -> check cb h (Idx i)
        KeyValue InStaticTable i  -> return (hdrtbl, builder << Indexed i)
        KeyValue InDynamicTable i -> func i

check :: Ctx -> Header -> Naming -> IO Ctx
check (hdrtbl,builder) h@(k,v) x
  | k `elem` headersNotToIndex = do
      let builder' = builder << Literal NotAdd x v
      return (hdrtbl, builder')
  | otherwise = do
      let e = toEntry h
      hdrtbl' <- insertEntry e hdrtbl
      let builder' = builder << Literal Add x v
      return (hdrtbl', builder')

headersNotToIndex :: [HeaderName]
headersNotToIndex = [
    ":path"
  , "content-length"
  , "location"
  , "etag"
  , "set-cookie"
  ]
