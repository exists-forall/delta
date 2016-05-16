module TestUtils (allSatisfy) where

import Test.Hspec
import Test.Hspec.Core.Spec (Result(Success, Fail))

-- Is there really no built-in way to do this in Hspec?
allSatisfy :: (Show a) => [a] -> (a -> Bool) -> Result
allSatisfy [] _ = Success
allSatisfy (x : xs) f =
  if f x
    then allSatisfy xs f
    else Fail ("Predicate failed on item: " ++ show x)
