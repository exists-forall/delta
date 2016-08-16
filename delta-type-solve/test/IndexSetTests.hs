{-# LANGUAGE TupleSections #-}

module IndexSetTests (test) where

import TestUtils (allSatisfy)
import IndexSet

import Test.Hspec

test :: Spec
test = describe "IndexSet" $ do
  describe "empty" $ do
    it "has no members" $
      [0..100] `allSatisfy` (not . (`member` empty))

  let testIndices = [0, 5, 31, 32, 33, 63, 64, 65, 200]

  describe "singleton" $ do
    it "produces a set with a particular index as a member" $
      testIndices `allSatisfy` (\i -> i `member` (singleton i))

    let distinctPairs = testIndices >>= (\i -> (i,) <$> ([1..i-1] ++ [i+1..300]))

    it "produces a set with no other indices as members" $
      distinctPairs `allSatisfy` (\(i, j) -> not (j `member` (singleton i)))

  describe "fromIndices" $ do
    let set = fromIndices testIndices

    it "produces a set with particular indices as members" $
      testIndices `allSatisfy` (`member` set)

    it "produces a set with no other indices as members" $
      (filter (not . (`elem` testIndices)) [0..300]) `allSatisfy` (not . (`member` set))

  describe "indices" $ do
    it "returns all indices of a set" $
      indices (fromIndices testIndices) `shouldBe` testIndices

  describe "union" $ do
    let indices1 = [0, 5, 65]
    let indices2 = [2, 5, 35]
    let set = (fromIndices indices1) `union` (fromIndices indices2)
    it "produces a set which contains all indices in either of two sets" $
      (indices1 ++ indices2) `allSatisfy` (`member` set)

  describe "intersection" $ do
    let indices1 = [0, 3, 5, 35, 36, 65]
    let indices2 = [0, 5, 6, 36, 130]
    let expectedIncluded = [0, 5, 36]
    let expectedExcluded = [3, 6, 35, 65]
    let set = (fromIndices indices1) `intersection` (fromIndices indices2)

    it "produces a set which contains all indices in both of two sets" $
      expectedIncluded `allSatisfy` (`member` set)

    it "produces a set which excludes indices not in both of two sets" $
      expectedExcluded `allSatisfy` (not . (`member` set))

    it "works correctly with set equality" $
      set `shouldBe` (fromIndices expectedIncluded)

  describe "set difference (\\\\)" $ do
    let indices1 = [0, 3, 5, 35, 36, 65, 128]
    let indices2 = [0, 5, 6, 36, 128]
    let expectedIncluded = [3, 35, 65]
    let expectedExcluded = [0, 5, 6, 36, 128]
    let set = (fromIndices indices1) \\ (fromIndices indices2)

    it "produces a set which contains all indices in a source set and not in an excluded set" $ do
      expectedIncluded `allSatisfy` (`member` set)

    it "produces a set which excludes all indices not in a source set or in an excluded set" $ do
      expectedExcluded `allSatisfy` (not . (`member` set))

    it "works correctly with set equality" $
      set `shouldBe` (fromIndices expectedIncluded)
