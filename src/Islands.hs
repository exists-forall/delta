{-# language ScopedTypeVariables #-}

module Islands {-# DEPRECATED "Use Families instead" #-}
  ( Connection (Connection)
  , Island
  , findIslands
  )
where

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Control.Monad.State as State
import Control.Monad.State (State)

data Connection a = Connection a a deriving (Eq, Ord, Show)

type Island = Int
type Index = Int

addConnectionUnidirectional :: (Ord a) => a -> a -> Map a [a] -> Map a [a]
addConnectionUnidirectional x y m =
  case Map.lookup x m of
    Just connections -> Map.insert x (y : connections) m
    Nothing -> Map.insert x [y] m

addConnection :: (Ord a) => Map a [a] -> Connection a -> Map a [a]
addConnection m (Connection x y) =
  addConnectionUnidirectional y x $ addConnectionUnidirectional x y m

buildConnectionMap :: (Ord a) => [Connection a] -> Map a [a]
buildConnectionMap = foldl addConnection Map.empty

data IslandState a = IslandState
  { connectionMap :: Map a [a]
  , islandMap :: Map a Island
  }

findIslands :: forall a. (Ord a) => [Connection a] -> Map a Island
findIslands connections =
  let
    addToIsland :: Island -> a -> State (IslandState a) ()
    addToIsland island node = do
      current <- State.get
      case Map.lookup node (connectionMap current) of
        Just connections -> do
          let connectionMap' = Map.delete node (connectionMap current)
          let islandMap' = Map.insert node island (islandMap current)
          State.put (current { connectionMap = connectionMap', islandMap = islandMap' })
          mapM_ (addToIsland island) connections
        Nothing -> return ()

    addRemainingIslands :: Int -> State (IslandState a) ()
    addRemainingIslands existingIslandCount = do
      remainingConnections <- connectionMap <$> State.get
      if Map.null remainingConnections
        then return ()
        else do
          addToIsland existingIslandCount (fst (Map.findMin remainingConnections))
          addRemainingIslands (existingIslandCount + 1)

    initialState = IslandState
      { connectionMap = buildConnectionMap connections
      , islandMap = Map.empty
      }
  in
    islandMap (snd (State.runState (addRemainingIslands 0) initialState))
