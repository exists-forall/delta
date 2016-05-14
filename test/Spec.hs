module Main where

import qualified DirectedGraphTests
import qualified FamiliesTests
import qualified IndexSetTests
import qualified PosetSubsetSessionTests
import qualified TopoSortTests

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "TypeInference" $ do
    DirectedGraphTests.test
    FamiliesTests.test
    IndexSetTests.test
    PosetSubsetSessionTests.test
    TopoSortTests.test
