module TestUtils
  ( allSatisfy
  , allSame
  , allDifferent
  )
where

import Test.Hspec
import Test.Hspec.Core.Spec (Result(Success, Fail))

import qualified Data.Set as Set

-- Is there really no built-in way to do this in Hspec?
allSatisfy :: (Show a) => [a] -> (a -> Bool) -> Result
allSatisfy [] _ = Success
allSatisfy (x : xs) f =
  if f x
    then allSatisfy xs f
    else Fail ("Predicate failed on item: " ++ show x)

allSame :: (Eq a) => [a] -> Bool
allSame xs = and (zipWith (==) xs (tail xs))

allDifferent :: (Ord a) => [a] -> Bool
allDifferent xs = Set.size (Set.fromList xs) == length xs
