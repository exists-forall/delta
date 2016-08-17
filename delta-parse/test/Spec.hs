module Main where

import qualified ParseIdentTests

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "delta-parse" $ do
    ParseIdentTests.test
