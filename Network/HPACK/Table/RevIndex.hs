{-# LANGUAGE BangPatterns, OverloadedStrings, CPP, RecordWildCards #-}

module Network.HPACK.Table.RevIndex (
    RevIndex
  , newRevIndex
  , renewRevIndex
  , lookupRevIndex
  , insertRevIndex
  , deleteRevIndexList
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>), (<*>))
#endif
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Function (on)
import Data.IORef
import Data.List (groupBy)
import Network.HPACK.Table.Entry
import Network.HPACK.Table.Static
import Network.HPACK.Types

----------------------------------------------------------------

type ValueMap = Map HeaderValue HIndex
type KeyMap = Map HeaderName ValueMap

data RevIndex = RevIndex (IORef KeyMap)

----------------------------------------------------------------

staticRevIndex :: KeyMap
staticRevIndex = M.fromList $ map toEnt zs
  where
    toEnt (k, xs) = (k, M.fromList xs)
    zs = map extract $ groupBy ((==) `on` fst) lst
      where
        lst = zipWith (\(k,v) i -> (k,(v,i))) staticTableList $ map SIndex [1..]
        extract xs = (fst (head xs), map snd xs)

anyValue :: ValueMap -> HIndex
anyValue inner = idx
  where
    [_,root,_] = M.splitRoot inner
    [(_,idx)] = M.toList root

----------------------------------------------------------------

newRevIndex :: IO RevIndex
newRevIndex = RevIndex <$> newIORef staticRevIndex

renewRevIndex :: RevIndex -> IO ()
renewRevIndex (RevIndex ref) = writeIORef ref staticRevIndex

lookupRevIndex :: HeaderName
               -> HeaderValue
               -> (HIndex -> IO ())
               -> (HeaderValue -> Entry -> HIndex -> IO ())
               -> (HeaderName -> HeaderValue -> Entry -> IO ())
               -> (HeaderValue -> HIndex -> IO ())
               -> RevIndex
               -> IO ()
lookupRevIndex k v fa fb fc fd (RevIndex ref) = do
    outer <- readIORef ref
    case M.lookup k outer of
      Nothing
        | shouldBeIndexed -> fc k v ent
        | otherwise       -> error "lookupRevIndex"
      Just inner
        | shouldBeIndexed -> case M.lookup v inner of
            Nothing       -> fb v ent (anyValue inner)
            Just i        -> fa i
        | otherwise       -> fd v (anyValue inner)

  where
    shouldBeIndexed = k `notElem` headersNotToIndex
    ent = toEntry (k,v)

----------------------------------------------------------------

{-# INLINE insertRevIndex #-}
insertRevIndex :: Entry -> HIndex -> RevIndex -> IO ()
insertRevIndex (Entry _ (k,v)) i (RevIndex ref) =
    atomicModifyIORef' ref $ \outer -> (M.alter f k outer, ())
  where
    f Nothing      = Just $ M.singleton v i
    f (Just inner) = Just $ M.insert v i inner

{-# INLINE deleteRevIndex #-}
deleteRevIndex :: RevIndex -> Entry -> IO ()
deleteRevIndex (RevIndex ref) (Entry _ (k,v)) =
    atomicModifyIORef' ref $ \outer -> (M.alter f k outer, ())
  where
    f Nothing         = Nothing
    f (Just inner)
      | M.null inner' = Nothing
      | otherwise     = Just inner'
      where
         inner' = M.delete v inner

{-# INLINE deleteRevIndexList #-}
deleteRevIndexList :: [Entry] -> RevIndex -> IO ()
deleteRevIndexList es rev = mapM_ (deleteRevIndex rev) es

----------------------------------------------------------------

headersNotToIndex :: [HeaderName]
headersNotToIndex = [
    ":path"
  , "content-length"
  , "location"
  , "etag"
  , "set-cookie"
  ]

