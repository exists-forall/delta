module TestUtils
  ( allSatisfy
  , allSame
  , allDifferent
  , iff
  , shouldBeUnordered
  )
where

import Test.Hspec
import Test.Hspec.Core.Spec (Result(Success, Fail))

import qualified Data.Set as Set

-- Is there really no built-in way to do this in Hspec?
-- Also, should I be using Expectation instead of Result?
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

iff :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
iff cond1 cond2 x = cond1 x == cond2 x

shouldBeUnordered :: (Show a, Ord a) => [a] -> [a] -> Expectation
shouldBeUnordered xs ys = Set.fromList xs `shouldBe` Set.fromList ys
