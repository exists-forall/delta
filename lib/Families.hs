module Families (Family, Index, buildFamilies) where

import CollectionUtils (consInsert)

import qualified Data.Map as Map
import Data.Map (Map)

type Family = Int
type Index = Int

type Connections a = Map a [a]

addConnection :: (Ord a) => Connections a -> (a, a) -> Connections a
addConnection connections (x, y) = consInsert x y $ consInsert y x $ connections

buildConnections :: (Ord a) => [(a, a)] -> Connections a
buildConnections = foldl addConnection Map.empty

type Families a = Map a (Family, Index)

registerFamilyWithSeed' :: (Ord a) => Family -> (Connections a, Index, Families a) -> a -> (Connections a, Index, Families a)
registerFamilyWithSeed' family (connections, indexCount, families) seed =
  case Map.lookup seed connections of
    Just connected ->
      let
        connections' = Map.delete seed connections
        families' = Map.insert seed (family, indexCount) families
        indexCount' = indexCount + 1
      in
        foldl (registerFamilyWithSeed' family) (connections', indexCount', families') connected

    Nothing -> (connections, indexCount, families)

registerFamilyWithSeed :: (Ord a) => (Family, Connections a, Families a) -> a -> (Family, Connections a, Families a)
registerFamilyWithSeed (familyCount, connections, families) seed =
  let
    (connections', _, families') = registerFamilyWithSeed' familyCount (connections, 0, families) seed
    familyCount' = familyCount + 1
  in
    (familyCount', connections', families')

registerNextFamily :: (Ord a) => (Family, Connections a, Families a) -> (Family, Connections a, Families a)
registerNextFamily (familyCount, connections, families) =
  registerFamilyWithSeed (familyCount, connections, families) (fst (Map.findMin connections))

registerAllFamilies :: (Ord a) => Connections a -> Families a
registerAllFamilies allConnections =
  families where
    (_, _, families) =
      until
        (\(_, connections, _) -> Map.null connections)
        registerNextFamily
        (0, allConnections, Map.empty)

buildFamilies :: (Ord a) => [(a, a)] -> Families a
buildFamilies = registerAllFamilies . buildConnections
