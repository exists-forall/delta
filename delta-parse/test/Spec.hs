module Main where

import qualified ParseIdentTests
import qualified ParseExprTests
import qualified DeltaPrecedenceTests

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "delta-parse" $ do
    ParseIdentTests.test
    ParseExprTests.test
    DeltaPrecedenceTests.test
