module Main where

import qualified ParseIdentTests
import qualified ParseExprTests
import qualified ParsePatTests
import qualified DeltaPrecedenceTests
import qualified ParseTypeTests

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "delta-parse" $ do
    ParseIdentTests.test
    ParseExprTests.test
    ParsePatTests.test
    ParseTypeTests.test
    DeltaPrecedenceTests.test
