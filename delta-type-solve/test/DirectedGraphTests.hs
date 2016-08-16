module DirectedGraphTests (test) where

import DirectedGraph

import Test.Hspec

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set

edgesEqualUpToPermutation :: (Show a, Ord a) => Map a [a] -> Map a [a] -> Expectation
edgesEqualUpToPermutation xs ys =
  (Map.map Set.fromList xs) `shouldBe` (Map.map Set.fromList ys)

test :: Spec
test = describe "DirectedGraph" $ do
  let
    graph = buildDirectedGraph
      [ 'A' `EdgeTo` 'B'
      , 'B' `EdgeTo` 'C'
      , 'D' `EdgeTo` 'E'
      , 'D' `EdgeTo` 'G'
      , 'F' `EdgeTo` 'E'
      , 'F' `EdgeTo` 'G'
      , 'E' `EdgeTo` 'E'
      , 'G' `EdgeTo` 'F'
      ]

  let
    expectedIncoming = Map.fromList
      [ ('B', ['A'])
      , ('C', ['B'])
      , ('E', ['D', 'F', 'E'])
      , ('G', ['F', 'D'])
      , ('F', ['G'])
      ]

  let
    expectedOutgoing = Map.fromList
      [ ('A', ['B'])
      , ('B', ['C'])
      , ('D', ['E', 'G'])
      , ('F', ['E', 'G'])
      , ('E', ['E'])
      , ('G', ['F'])
      ]

  it "finds incoming edges" $ edgesEqualUpToPermutation expectedIncoming (incomingEdges graph)

  it "finds outgoing edges" $ edgesEqualUpToPermutation expectedOutgoing (outgoingEdges graph)
