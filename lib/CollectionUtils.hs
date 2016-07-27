module CollectionUtils
  ( foldInsert
  , consInsert

  , fromListWithFold
  , fromListWithCons

  , mapFromListWithKeyM
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

mapFromListWithKeyM :: (Ord k, Monad m) => (k -> a -> a -> m a) -> [(k, a)] -> m (Map k a)
mapFromListWithKeyM f =
  mapM id .
  Map.fromListWithKey (\k a b -> join (f k <$> a <*> b)) .
  map (second return)
