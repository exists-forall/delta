{-# language FlexibleContexts #-}

module TopoSort (topoSort) where

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import Data.Maybe (fromMaybe)

import qualified Control.Monad.State as State

import Control.Monad.Trans (lift)

data TopoSortState a = TopoSortState
  { remaining :: Set a
  , inProgress :: Set a
  , reverseOrder :: [a]
  }

topoSort :: (Ord a) => Map a [a] -> Maybe [a]
topoSort depMap =
  let
    initialState = TopoSortState
      { remaining = Set.fromList (concatMap (\(x, deps) -> x : deps) (Map.toList depMap))
      , inProgress = Set.empty
      , reverseOrder = []
      }

    begin node = do
      current <- State.get
      let inProgress' = Set.insert node (inProgress current)
      State.put (current { inProgress = inProgress' })

    complete node = do
      current <- State.get
      let remaining' = Set.delete node (remaining current)
      let inProgress' = Set.delete node (inProgress current)
      let reverseOrder' = node : reverseOrder current
      State.put (current { remaining = remaining', inProgress = inProgress', reverseOrder = reverseOrder' })

    process node = do
      completed <- (not . Set.member node . remaining) <$> State.get
      if completed
        then return ()
        else do
          isInProgress <- (Set.member node . inProgress) <$> State.get
          if isInProgress
            then lift Nothing
            else do
              begin node
              let deps = fromMaybe [] (Map.lookup node depMap)
              mapM_ process deps
              complete node

    processAll = do
      currentRemaining <- remaining <$> State.get
      if Set.null currentRemaining
        then return ()
        else do
          process (Set.findMin currentRemaining)
          processAll

    in
      (reverse . reverseOrder . snd) <$> (State.runStateT processAll initialState)
