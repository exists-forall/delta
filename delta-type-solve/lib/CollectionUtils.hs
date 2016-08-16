module CollectionUtils
  ( foldInsert
  , consInsert

  , fromListWithFold
  , fromListWithCons

  , transferValues
  , heteroUnionWithKey
  , unionWithKeyM
  , mapFromListWithKeyM

  , tryExactZip
  )
where

import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (second)

import qualified Data.Map as Map
import Data.Map (Map)

foldInsert :: (Ord k) => (a -> b -> a) -> a -> k -> b -> Map k a -> Map k a
foldInsert f base key x m =
  let accum = fromMaybe base $ Map.lookup key m
  in Map.insert key (f accum x) m

consInsert :: (Ord k) => k -> a -> Map k [a] -> Map k [a]
consInsert = foldInsert (flip (:)) []

fromListWithFold :: (Ord k) => (a -> b -> a) -> a -> [(k, b)] -> Map k a
fromListWithFold f base = foldl (flip (uncurry (foldInsert f base))) Map.empty

fromListWithCons :: (Ord k) => [(k, a)] -> Map k [a]
fromListWithCons = fromListWithFold (flip (:)) []

-- Return an updated version of the second argument by using values from the first where available.
transferValues :: (Ord k) => Map k a -> Map k a -> Map k a
transferValues replacements =
  Map.mapWithKey (\k v -> fromMaybe v $ Map.lookup k replacements)

data ValuePair a b
  = Both a b
  | OnlyLeft a
  | OnlyRight b

combineValuePair :: ValuePair a b -> ValuePair a b -> ValuePair a b
combineValuePair (OnlyLeft x) (OnlyRight y) = Both x y
combineValuePair _ _ =
  error "This function is only used internally, and no other case should ever be called"

handleValuePair ::
  (k -> a -> c) ->
  (k -> b -> c) ->
  (k -> a -> b -> c) ->
  k ->
  ValuePair a b ->
  c

handleValuePair g _ _ k (OnlyLeft x) = g k x
handleValuePair _ h _ k (OnlyRight y) = h k y
handleValuePair _ _ f k (Both x y) = f k x y

-- TODO: rewrite in terms of mergeWithKey
heteroUnionWithKey ::
  (Ord k) =>
  (k -> a -> c) ->
  (k -> b -> c) ->
  (k -> a -> b -> c) ->
  Map k a ->
  Map k b ->
  Map k c

heteroUnionWithKey f g h m1 m2 =
  Map.mapWithKey (handleValuePair f g h) $
  Map.unionWith
    combineValuePair
    (Map.map OnlyLeft m1)
    (Map.map OnlyRight m2)

unionWithKeyM :: (Ord k, Monad m) => (k -> a -> a -> m a) -> Map k a -> Map k a -> m (Map k a)
unionWithKeyM f m1 m2 =
  mapM id $
  heteroUnionWithKey (const return) (const return) f m1 m2

mapFromListWithKeyM :: (Ord k, Monad m) => (k -> a -> a -> m a) -> [(k, a)] -> m (Map k a)
mapFromListWithKeyM f =
  mapM id .
  Map.fromListWithKey (\k a b -> join (f k <$> a <*> b)) .
  map (second return)

tryExactZip :: [a] -> [a] -> Maybe [(a, a)]
tryExactZip [] [] = Just []
tryExactZip (x:xs) (y:ys) = fmap ((x, y) :) $ tryExactZip xs ys
tryExactZip _ _ = Nothing
