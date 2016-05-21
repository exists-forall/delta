{-# LANGUAGE Rank2Types #-}

module PosetSubsetSession
  ( Relation (ChildOf)
  , Poset
  , Node
  , NodeValue

  , buildPoset
  , runSession

  , topNode
  , botNode

  , Lower
  , Upper
  , Subset

  , extendWithChildren
  , extendWithParents
  , member
  , intersection
  , maximal
  , minimal
  )
where

import qualified Data.Map as Map
import Data.Map (Map)

import qualified IndexSet
import IndexSet (IndexSet)

import HelpfulUnsafeLookup

import qualified TopoSort
import qualified Families
import qualified DirectedGraph

data Relation a = a `ChildOf` a deriving (Eq, Ord, Show)

type Family = Families.Family
type Index = Families.Index

data Poset a = Poset
  { nodesForValues :: Map a (Family, Index)
  -- should be a mirror-image of `nodesForValues`
  , valuesForNodes :: Map (Family, Index) a

  -- `childrenForNodes` and `nodesForChildren` should conceptially be mirror-images of each other.
  -- Every node with a family and index in `nodes` also has an entry in `childrenForNodes`, and vice-versa.
  , childrenForNodes :: Map (Family, Index) IndexSet
  , nodesForChildren :: Map (Family, IndexSet) Index

  -- `parentsForNodes` and `nodesForParents` should conceptially be mirror-images of each other.
  -- Every node with a family and index in `nodes` also has an entry in `parentsForNodes`, and vice-versa.
  , parentsForNodes :: Map (Family, Index) IndexSet
  , nodesForParents :: Map (Family, IndexSet) Index
  }
  deriving (Show)

-- All nodes of a given `s` should have the same `Poset`
data Node s a
  = SolitaryNode a
  | TopNode -- ⊤
  | BotNode -- ⊥
  | FamilyNode (Poset a) Family Index

data NodeValue a
  = NodeValue a
  | TopNodeValue
  | BotNodeValue
  deriving (Eq, Ord, Show)

topNode :: Node s a
topNode = TopNode

botNode :: Node s a
botNode = BotNode

-- The only purpose of this data type is to auto-derive Eq and Ord instances for it.
-- We then use those Eq and Ord instances to define the equivalent operations on normal Nodes,
-- by projecting from Nodes to ComparisonNodes.
-- Eq and Ord instances cannot be derived for Nodes directly because that would involve comparing the
-- Posets of FamilyNodes, which is both computationally wasteful (because they should be identical)
-- and impossible, because there are no Eq or Ord instances for Poset.
data ComparisonNode a
  = SolitaryComparisonNode a
  | TopComparisonNode
  | BotComparisonNode
  | FamilyComparisonNode Family Index
  deriving (Eq, Ord)

toComparisonNode :: Node s a -> ComparisonNode a
toComparisonNode (SolitaryNode val) = SolitaryComparisonNode val
toComparisonNode TopNode = TopComparisonNode
toComparisonNode BotNode = BotComparisonNode
toComparisonNode (FamilyNode _ family index) = FamilyComparisonNode family index

instance (Eq a) => Eq (Node s a) where
  a == b = toComparisonNode a == toComparisonNode b

instance (Ord a) => Ord (Node s a) where
  compare a b = compare (toComparisonNode a) (toComparisonNode b)

data Lower
data Upper
-- All subsets of a given `s` should have the same `Poset`
data Subset form s a
  = Universal -- ↓{⊤}, ↑{⊥}
  | Trivial   -- ↓{⊥}, ↑{⊤}
  | SolitarySubset a
  | FamilialSubset (Poset a) Family IndexSet

-- Smart constructor to handle case of empty index set.
-- not strictly necessary, but may have some performance benefits,
-- and may improve error messages in the type inference engine.
-- Further investigation is needed.  It may turn out this should be removed.
familialSubset :: (Ord a) => Poset a -> Family -> IndexSet -> Subset form s a
familialSubset poset family indices =
  if indices == IndexSet.empty
    then Trivial
    else FamilialSubset poset family indices

extendWithChildren :: (Ord a) => Node s a -> Subset Lower s a
extendWithChildren TopNode = Universal
extendWithChildren BotNode = Trivial
extendWithChildren (SolitaryNode x) = (SolitarySubset x)
extendWithChildren (FamilyNode poset family index) =
  FamilialSubset poset family ((childrenForNodes poset) ! (family, index))

extendWithParents :: (Ord a) => Node s a -> Subset Upper s a
extendWithParents TopNode = Trivial
extendWithParents BotNode = Universal
extendWithParents (SolitaryNode x) = (SolitarySubset x)
extendWithParents (FamilyNode poset family index) =
  FamilialSubset poset family ((parentsForNodes poset) ! (family, index))

member :: (Ord a) => (Node s a) -> Subset form s a -> Bool
member _ Universal = True
member (SolitaryNode x) (SolitarySubset y) = x == y
member (FamilyNode _ family index) (FamilialSubset _ setFamily setIndices) =
  family == setFamily && index `IndexSet.member` setIndices
member _ _ = False

intersection :: (Ord a) => Subset form s a -> Subset form s a -> Subset form s a

intersection Universal s = s
intersection s Universal = s

intersection _ Trivial = Trivial
intersection Trivial _ = Trivial

intersection (SolitarySubset x) (SolitarySubset y) =
  if x == y
    then SolitarySubset x
    else Trivial

intersection (FamilialSubset poset family1 indices1) (FamilialSubset _ family2 indices2) =
  if family1 == family2
    then familialSubset poset family1 (indices1 `IndexSet.intersection` indices2)
    else Trivial

intersection _ _ = Trivial

extremal :: (Poset a -> Map (Family, Index) IndexSet) -> (Poset a -> Map (Family, IndexSet) Index) -> Subset form s a -> [Node s a]
extremal _ _ Universal = [TopNode]
extremal _ _ Trivial = [BotNode]
extremal _ _ (SolitarySubset x) = [SolitaryNode x]
extremal extensionForNodes nodesForExtension (FamilialSubset poset family indices) =
  case Map.lookup (family, indices) (nodesForExtension poset) of
    Just index -> [FamilyNode poset family index]
    Nothing ->
      let
        possibleNodes = IndexSet.indices indices
        strictChildren index =
          let inclusiveChildren = (extensionForNodes poset) ! (family, index)
          in inclusiveChildren IndexSet.\\ (IndexSet.singleton index)
        excludedChildren = foldl IndexSet.union IndexSet.empty $ map strictChildren possibleNodes
        extremalIndices = IndexSet.indices (indices IndexSet.\\ excludedChildren)
      in
        map (FamilyNode poset family) extremalIndices

maximal :: Subset Lower s a -> [Node s a]
maximal = extremal childrenForNodes nodesForChildren

minimal :: Subset Upper s a -> [Node s a]
minimal = extremal parentsForNodes nodesForParents

type ImmediatePredecessors = Map (Family, Index) [(Family, Index)]
type AllPredecessors = Map (Family, Index) IndexSet

registerAllPredecessors :: ImmediatePredecessors -> AllPredecessors -> (Family, Index) -> AllPredecessors
registerAllPredecessors predecessors allPredecessors (family, index) =
  let
    transitivePredecessors = map (allPredecessors !) (Map.findWithDefault [] (family, index) predecessors)
    thisPredecessors = foldl IndexSet.union (IndexSet.singleton index) transitivePredecessors
  in
    Map.insert (family, index) thisPredecessors allPredecessors

buildTransitivePredecessors :: ImmediatePredecessors -> [(Family, Index)] -> AllPredecessors
buildTransitivePredecessors predecessors order =
  foldl (registerAllPredecessors predecessors) Map.empty order

invertMap :: (Ord b) => Map a b -> Map b a
invertMap = Map.fromList . map (\(x, y) -> (y, x)) . Map.toList

invertPredecessorMap :: AllPredecessors -> Map (Family, IndexSet) Index
invertPredecessorMap =
  Map.fromList . map (\((family, index), preds) -> ((family, preds), index)) . Map.toList

buildPoset :: (Show a, Ord a) => [Relation a] -> Maybe (Poset a)
buildPoset relations =
  do
    -- Build Family map
    let nodes = Families.buildFamilies (map relationToConnection relations)
    let nodeForValue = (!) nodes
    let nodeRelations = map (\(a `ChildOf` b) -> (nodeForValue a) `ChildOf` (nodeForValue b)) relations

    -- Build Children / Parents map
    let edgesFromChildrenToParents = map relationToEdge nodeRelations
    let graphFromChildrenToParents = DirectedGraph.buildDirectedGraph edgesFromChildrenToParents

    childrenBeforeParents <- TopoSort.topoSort (DirectedGraph.incomingEdges graphFromChildrenToParents)
    let transitiveChildren = buildTransitivePredecessors (DirectedGraph.incomingEdges graphFromChildrenToParents) childrenBeforeParents

    let parentsBeforeChildren = reverse childrenBeforeParents
    let transitiveParents = buildTransitivePredecessors (DirectedGraph.outgoingEdges graphFromChildrenToParents) parentsBeforeChildren

    return $ Poset
      { nodesForValues = nodes
      , valuesForNodes = invertMap nodes

      , childrenForNodes = transitiveChildren
      , nodesForChildren = invertPredecessorMap transitiveChildren

      , parentsForNodes = transitiveParents
      , nodesForParents = invertPredecessorMap transitiveParents
      }

-- Yes, these conversion functions are trivial, but having different types for all of these things
-- seems worth it for name clarity.  Simple tuples obsucre the meaning of pairs when order is significant.
relationToConnection :: Relation a -> (a, a)
relationToConnection (child `ChildOf` parent) = (child, parent)

relationToEdge :: Relation a -> DirectedGraph.Edge a
relationToEdge (child `ChildOf` parent) = child `DirectedGraph.EdgeTo` parent

runSession :: (Ord a) => (forall s. (a -> Node s a) -> (Node s a -> NodeValue a) -> b) -> Poset a -> b
runSession body poset =
  let
    toNode val =
      case Map.lookup val (nodesForValues poset) of
        Just (family, index) -> FamilyNode poset family index
        Nothing -> SolitaryNode val

    fromNode (SolitaryNode val) = NodeValue val
    fromNode TopNode = TopNodeValue
    fromNode BotNode = BotNodeValue
    fromNode (FamilyNode _ family index) =
      NodeValue ((valuesForNodes poset) Map.! (family, index))
  in
    body toNode fromNode
