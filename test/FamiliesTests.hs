module FamiliesTests (test) where

import Families

import TestUtils

import Test.Hspec

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map)

test :: Spec
test = describe "Families" $ do
  let
    graph =
      [ ('A', 'B')
      , ('B', 'C')
      , ('D', 'E')
      , ('D', 'G')
      , ('E', 'E')
      , ('F', 'G')
      , ('F', 'E')
      , ('G', 'F')
      , ('E', 'E')
      ]

  let component1Nodes = ['A', 'B', 'C']
  let component2Nodes = ['D', 'E', 'F', 'G']

  let annotations = buildFamilies graph

  let familiesOf = map (fst . (annotations Map.!))
  let component1Families = familiesOf component1Nodes
  let component2Families = familiesOf component2Nodes

  it "assigns the same family id to all nodes in a connected component (1)" $
    component1Families `shouldSatisfy` allSame

  it "assigns the same family id to all nodes in a connected component (2)" $
    component2Families `shouldSatisfy` allSame

  it "assigns different family ids to nodes in different connected components" $
    (head component1Families, head component2Families) `shouldSatisfy` (uncurry (/=))

  let indicesOf = map (snd . (annotations Map.!))
  let component1Indices = indicesOf component1Nodes
  let component2Indices = indicesOf component2Nodes

  it "assigns different indices to different nodes in the same connected component (1)" $
    component1Indices `shouldSatisfy` allDifferent

  it "assigns different indices to different nodes in the same connected component (2)" $
    component2Indices `shouldSatisfy` allDifferent
