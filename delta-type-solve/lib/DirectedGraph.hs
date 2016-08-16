module DirectedGraph
  ( DirectedGraph (DirectedGraph, incomingEdges, outgoingEdges)
  , Edge (EdgeTo)
  , buildDirectedGraph
  )
where

import qualified Data.Map as Map
import Data.Map (Map)

data Edge a = a `EdgeTo` a deriving (Eq, Ord, Show)

reverseEdge :: Edge a -> Edge a
reverseEdge (a `EdgeTo` b) = b `EdgeTo` a

data DirectedGraph a = DirectedGraph
  { incomingEdges :: Map a [a]
  , outgoingEdges :: Map a [a]
  }
  deriving (Eq, Ord, Show)

addEdge :: (Ord a) => Map a [a] -> Edge a -> Map a [a]
addEdge incomingMap (a `EdgeTo` b) =
      let
        newEdges = case Map.lookup b incomingMap of
          Just deps -> a : deps
          Nothing -> [a]
      in
        Map.insert b newEdges incomingMap

buildDirectedGraph :: (Ord a) => [Edge a] -> DirectedGraph a
buildDirectedGraph edges =
  DirectedGraph
    { incomingEdges = foldl addEdge Map.empty edges
    , outgoingEdges = foldl addEdge Map.empty (map reverseEdge edges)
    }
