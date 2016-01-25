{-# LANGUAGE BangPatterns, OverloadedStrings, CPP #-}

module Network.HPACK2.HeaderBlock.To (
    toHeaderBlock
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Network.HPACK2.Builder
import Network.HPACK2.HeaderBlock.HeaderField
import Network.HPACK2.Table
import Network.HPACK2.Types

type Ctx = Builder HeaderField
type Step = DynamicTable -> Ctx -> Header -> IO Ctx

-- | Encoding 'HeaderList' to 'HeaderBlock'.
toHeaderBlock :: CompressionAlgo
              -> DynamicTable
              -> HeaderList
              -> IO HeaderBlock
toHeaderBlock algo !dyntbl hs = do
    msiz <- needChangeTableSize dyntbl
    op <- case msiz of
        Keep -> do
            return id
        Change lim -> do
            renewDynamicTable lim dyntbl
            return (ChangeTableSize lim :)
        Ignore lim -> do
            resetLimitForEncoding dyntbl
            return (ChangeTableSize lim :)
    op <$> toHeaderBlock' algo dyntbl hs

toHeaderBlock' :: CompressionAlgo
               -> DynamicTable
               -> HeaderList
               -> IO HeaderBlock
toHeaderBlock' Naive  !dyntbl hs = encodeLoop naiveStep  hs dyntbl empty
toHeaderBlock' Static !dyntbl hs = encodeLoop staticStep hs dyntbl empty
toHeaderBlock' Linear !dyntbl hs = encodeLoop linearStep hs dyntbl empty

----------------------------------------------------------------

encodeFinal :: Ctx -> IO HeaderBlock
encodeFinal !builder = return $! run builder

encodeLoop :: Step
           -> HeaderList
           -> DynamicTable
           -> Ctx
           -> IO HeaderBlock
encodeLoop step (h:hs) !dyntbl ctx = step dyntbl ctx h >>= encodeLoop step hs dyntbl
encodeLoop _    []     _       ctx = encodeFinal ctx

----------------------------------------------------------------

naiveStep :: Step
naiveStep _ !builder (k,v) = do
    let builder' = builder << Literal NotAdd (Lit k) v
    return builder'

----------------------------------------------------------------

staticStep :: Step
staticStep dyntbl !builder h@(k,v) = do
    x <- lookupTable h dyntbl
    let b = case x of
            None                      -> Literal NotAdd (Lit k) v
            KeyOnly  InStaticTable i  -> Literal NotAdd (Idx i) v
            KeyOnly  InDynamicTable _ -> Literal NotAdd (Lit k) v
            KeyValue InStaticTable i  -> Literal NotAdd (Idx i) v
            KeyValue InDynamicTable _ -> Literal NotAdd (Lit k) v
        builder' = builder << b
    return builder'

----------------------------------------------------------------
-- A simple encoding strategy to reset the reference set first
-- by 'Index 0' and uses indexing as much as possible.

linearStep :: Step
linearStep dyntbl !builder h = smartStep linear dyntbl builder h
  where
    linear i = return (builder << Indexed i)

----------------------------------------------------------------

smartStep :: (Index -> IO Ctx) -> Step
smartStep func dyntbl !builder h@(k,_) = do
    cache <- lookupTable h dyntbl
    case cache of
        None                      -> check dyntbl builder h (Lit k)
        KeyOnly  InStaticTable i  -> check dyntbl builder h (Idx i)
        KeyOnly  InDynamicTable i -> check dyntbl builder h (Idx i)
        KeyValue InStaticTable i  -> return (builder << Indexed i)
        KeyValue InDynamicTable i -> func i

check :: DynamicTable -> Ctx -> Header -> Naming -> IO Ctx
check dyntbl builder h@(k,v) x
  | k `elem` headersNotToIndex = do
      let builder' = builder << Literal NotAdd x v
      return builder'
  | otherwise = do
      let e = toEntry h
      insertEntry e dyntbl
      let builder' = builder << Literal Add x v
      return builder'

headersNotToIndex :: [HeaderName]
headersNotToIndex = [
    ":path"
  , "content-length"
  , "location"
  , "etag"
  , "set-cookie"
  ]
