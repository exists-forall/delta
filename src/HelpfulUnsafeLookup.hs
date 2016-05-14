module HelpfulUnsafeLookup where

import qualified Data.Map as Map
import Data.Map (Map)

import GHC.Stack (errorWithStackTrace)

(!) :: (Show k, Show a, Ord k) => Map k a -> k -> a
m ! k =
  case Map.lookup k m of
    Just x -> x
    Nothing -> errorWithStackTrace ("Key `" ++ show k ++ "` is not in map `" ++ show m ++ "`")
