{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Network.HPACK.HeaderBlock.To (
    toHeaderBlock
  ) where

import Control.Arrow (second)
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
toHeaderBlock algo !dyntbl hs = do
    msiz <- needChangeTableSize dyntbl
    (dyntbl', op) <- case msiz of
        Keep -> do
            return (dyntbl, id)
        Change lim -> do
            tbl <- renewDynamicTable lim dyntbl
            return (tbl, (ChangeTableSize lim :))
        Ignore lim -> do
            resetLimitForEncoding dyntbl
            return (dyntbl, (ChangeTableSize lim :))
    second op <$> toHeaderBlock' algo dyntbl' hs

toHeaderBlock' :: CompressionAlgo
              -> DynamicTable
              -> HeaderList
              -> IO (DynamicTable, HeaderBlock)
toHeaderBlock' Naive  !dyntbl hs = encodeLoop naiveStep  hs (dyntbl,empty)
toHeaderBlock' Static !dyntbl hs = encodeLoop staticStep hs (dyntbl,empty)
toHeaderBlock' Linear !dyntbl hs = encodeLoop linearStep hs (dyntbl,empty)

{-
do mlim <- readIORef limitForEncoding
   (dyn', op) <- case mlim of
      Nothing -> (dyn, id)
      Just lim
        | lim >= v -> (set Nothing dyn, id)
        | lim <  v -> (renew dyn, (change_to v:))
-}

----------------------------------------------------------------

encodeFinal :: Ctx -> IO (DynamicTable, HeaderBlock)
encodeFinal (!dyntbl, !builder) = return (dyntbl, run builder)

encodeLoop :: Step
           -> HeaderList
           -> Ctx
           -> IO (DynamicTable, HeaderBlock)
encodeLoop step (h:hs) !dyntbl = step dyntbl h >>= encodeLoop step hs
encodeLoop _    []     !dyntbl = encodeFinal dyntbl

----------------------------------------------------------------

naiveStep :: Step
naiveStep (!dyntbl,!builder) (k,v) = do
    let builder' = builder << Literal NotAdd (Lit k) v
    return (dyntbl, builder')

----------------------------------------------------------------

staticStep :: Step
staticStep (!dyntbl,!builder) h@(k,v) = return (dyntbl, builder')
  where
    b = case lookupTable h dyntbl of
        None                      -> Literal NotAdd (Lit k) v
        KeyOnly  InStaticTable i  -> Literal NotAdd (Idx i) v
        KeyOnly  InDynamicTable _ -> Literal NotAdd (Lit k) v
        KeyValue InStaticTable i  -> Literal NotAdd (Idx i) v
        KeyValue InDynamicTable _ -> Literal NotAdd (Lit k) v
    builder' = builder << b

----------------------------------------------------------------
-- A simple encoding strategy to reset the reference set first
-- by 'Index 0' and uses indexing as much as possible.

linearStep :: Step
linearStep cb@(!dyntbl,!builder) h = smartStep linear cb h
  where
    linear i = return (dyntbl,builder << Indexed i)

----------------------------------------------------------------

smartStep :: (Index -> IO Ctx) -> Step
smartStep func cb@(!dyntbl,!builder) h@(k,_) = do
    let cache = lookupTable h dyntbl
    case cache of
        None                      -> check cb h (Lit k)
        KeyOnly  InStaticTable i  -> check cb h (Idx i)
        KeyOnly  InDynamicTable i -> check cb h (Idx i)
        KeyValue InStaticTable i  -> return (dyntbl, builder << Indexed i)
        KeyValue InDynamicTable i -> func i

check :: Ctx -> Header -> Naming -> IO Ctx
check (dyntbl,builder) h@(k,v) x
  | k `elem` headersNotToIndex = do
      let builder' = builder << Literal NotAdd x v
      return (dyntbl, builder')
  | otherwise = do
      let e = toEntry h
      dyntbl' <- insertEntry e dyntbl
      let builder' = builder << Literal Add x v
      return (dyntbl', builder')

headersNotToIndex :: [HeaderName]
headersNotToIndex = [
    ":path"
  , "content-length"
  , "location"
  , "etag"
  , "set-cookie"
  ]
