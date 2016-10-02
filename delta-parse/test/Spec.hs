module Main where

import qualified ParseIdentTests
import qualified ParseExprTests
import qualified ParsePatTests
import qualified DeltaPrecedenceTests
import qualified ParseTypeTests
import qualified ParseDeclTests
import qualified ParseModuleTests

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "delta-parse" $ do
    ParseIdentTests.test
    ParseExprTests.test
    ParsePatTests.test
    ParseTypeTests.test
    ParseDeclTests.test
    ParseModuleTests.test
    DeltaPrecedenceTests.test
