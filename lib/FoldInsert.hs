module FoldInsert
  ( foldInsert
  , consInsert
  )
where

import Data.Maybe (fromMaybe)

import qualified Data.Map as Map
import Data.Map (Map)

foldInsert :: (Ord k) => (a -> b -> a) -> a -> k -> b -> Map k a -> Map k a
foldInsert f base key x m =
  let accum = fromMaybe base $ Map.lookup key m
  in Map.insert key (f accum x) m

consInsert :: (Ord k) => k -> a -> Map k [a] -> Map k [a]
consInsert = foldInsert (flip (:)) []