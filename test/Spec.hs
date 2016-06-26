module Main where

import qualified DirectedGraphTests
import qualified FamiliesTests
import qualified IndexSetTests
import qualified PosetSubsetSessionTests
import qualified TopoSortTests
import qualified UnifyTests
import qualified PropagateTests

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "TypeInference" $ do
    DirectedGraphTests.test
    FamiliesTests.test
    IndexSetTests.test
    PosetSubsetSessionTests.test
    TopoSortTests.test
    UnifyTests.test
    PropagateTests.test
