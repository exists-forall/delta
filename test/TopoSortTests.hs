{-# LANGUAGE TupleSections #-}

module TopoSortTests (test) where

import TopoSort

import qualified DirectedGraph
import DirectedGraph (Edge (EdgeTo))

import TestUtils (allSatisfy)

import Test.Hspec

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import Data.Maybe (fromMaybe)

verifyToposort :: (Ord a) => Map a [a] -> [a] -> Bool
verifyToposort dependencies order =
  let
    dependenciesOf = flip (Map.findWithDefault []) dependencies
    go completed [] = True
    go completed (x : xs) =
      all (`Set.member` completed) (dependenciesOf x) && go (Set.insert x completed) xs
  in
    go Set.empty order

containsDuplicates :: (Ord a) => [a] -> Bool
containsDuplicates =
  (/= Set.singleton 1) . Set.fromList . map snd . Map.toList . Map.fromListWith (+) . map (,1)

test :: Spec
test = describe "TopoSort" $ do
  let
    edges =
      [ (1 `EdgeTo`  4), (1 `EdgeTo`  5), (1 `EdgeTo` 6)
      , (2 `EdgeTo`  4), (2 `EdgeTo`  8), (3 `EdgeTo` 7)
      , (3 `EdgeTo`  8), (3 `EdgeTo` 10), (4 `EdgeTo` 5)
      , (4 `EdgeTo`  8), (4 `EdgeTo` 10), (5 `EdgeTo` 6)
      , (5 `EdgeTo`  9), (5 `EdgeTo` 10), (6 `EdgeTo` 7)
      , (6 `EdgeTo`  8), (6 `EdgeTo` 10), (8 `EdgeTo` 9)
      , (8 `EdgeTo` 10), (9 `EdgeTo` 10)
      ]

  let
    dependencies =
      DirectedGraph.incomingEdges (DirectedGraph.buildDirectedGraph edges)

  describe "verifyToposort" $ do
    let correctOrder = [3, 2, 1, 4, 5, 6, 8, 9, 10, 7]

    it "returns True for a correct ordering" $
      verifyToposort dependencies correctOrder `shouldBe` True

    it "returns False for an incorrect ordering" $
      verifyToposort dependencies (reverse correctOrder) `shouldBe` False

  describe "containsDuplicates" $ do
    it "returns True when a list contains duplicates" $
      containsDuplicates [1, 2, 3, 4, 10, 5, -6, 4, 0] `shouldBe` True

    it "returns False when a list contains no duplicates" $
      containsDuplicates [1, 2, 3, 4, 10, 5, -6, 0] `shouldBe` False

  describe "topoSort" $ do
    let order = topoSort dependencies

    it "produces a list where every item is preceded by its dependencies" $
      order `shouldSatisfy` (fromMaybe False . fmap (verifyToposort dependencies))

    let keysAndValues = Set.fromList (Map.toList dependencies >>= (uncurry (:)))

    it "produces a list containing every key and value in the dependencies map" $
      fmap Set.fromList order `shouldBe` Just keysAndValues

    it "produces a list which contains no duplicate items" $
      order `shouldSatisfy` (fromMaybe False . fmap (not . containsDuplicates))
