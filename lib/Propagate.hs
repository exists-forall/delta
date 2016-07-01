{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Propagate
  ( ChangeStatus (..)
  , ConstraintQuery
  , ConstraintEnforcer
  , Problem (..)

  , queryVar
  , solve
  )
where

import qualified Data.Map as Map

import qualified Data.Set as Set
import Data.Set (Set)

import Control.Monad.ST
import Data.STRef hiding (modifySTRef)

import Control.Monad.Trans.Except
import Control.Monad.Trans (lift)
import Control.Monad (forM, forM_)

import Data.Maybe (fromMaybe, fromJust)

import FoldInsert (fromListWithCons)

data ChangeStatus = Changed | Unchanged deriving (Eq, Ord, Show)

data ConstraintQuery var val a = ConstraintQuery
  { queryDependencies :: [var]
  , queryCompute :: (var -> (val, ChangeStatus)) -> a
  }

type ConstraintEnforcer var val err = ConstraintQuery var val (Either err [(var, val)])

data Problem var val err = Problem
  { problemInitialVals :: [(var, val)]
  , problemDefaultVal :: val
  , problemConstraints :: [ConstraintEnforcer var val err]
  }

queryVar :: var -> ConstraintQuery var val (val, ChangeStatus)
queryVar var = ConstraintQuery
  { queryDependencies = [var]
  , queryCompute = ($ var)
  }

instance Functor (ConstraintQuery var val) where
  fmap f (ConstraintQuery deps comp) =
    ConstraintQuery deps (f . comp)

instance Applicative (ConstraintQuery var val) where
  pure x = ConstraintQuery [] (const x)
  (ConstraintQuery deps1 comp1) <*> (ConstraintQuery deps2 comp2) =
    ConstraintQuery (deps1 ++ deps2) (comp1 <*> comp2)

newtype Version = Version Int deriving (Eq, Ord, Enum)

newtype ConstraintID = ConstraintID Int deriving (Eq, Ord, Enum)

data Constraint s var val err = Constraint
  { constraintEnforcer :: ConstraintEnforcer var val err
  , constraintLastSeenVersions :: var -> STRef s Version
  }

data Node s val = Node
  { nodeVal :: STRef s val
  , nodeVersion :: STRef s Version
  , nodeConstraints :: Set ConstraintID
  }

type Op s err = ExceptT err (ST s)

-- Note: returns a partial function!
assocsToLookupFunc :: (Ord a) => [(a, b)] -> (a -> b)
assocsToLookupFunc assocs =
  let m = Map.fromList assocs
  in fromJust . (`Map.lookup` m)

whileM :: (Monad m) => (m Bool) -> m () -> m ()
whileM cond body = do
  condVal <- cond
  if condVal
    then body >> whileM cond body
    else return ()

liftEither :: (Monad m) => Either e a -> ExceptT e m a
liftEither (Left e) = throwE e
liftEither (Right x) = return x

solve :: forall var val err. (Ord var, Eq val) => Problem var val err -> Either err (var -> Maybe val)
solve problem = runST $ runExceptT go where
  go :: forall s. Op s err (var -> Maybe val)
  go = do
    let indexedConstraints = zip [(ConstraintID 0)..] (problemConstraints problem)

    lookupConstraint <- fmap assocsToLookupFunc $
      forM indexedConstraints $ \(constraintID, enforcer) -> do
        let deps = queryDependencies enforcer
        depVersionAssocs <- forM deps $ \dep -> do
          versionRef <- lift $ newSTRef $ Version (-1)
          return (dep, versionRef)
        let lookupDepVersion = assocsToLookupFunc depVersionAssocs
        return (constraintID, Constraint enforcer lookupDepVersion)

    let
      allVarConstraints =
        fromListWithCons $
        concatMap (\(constraintID, enforcer) ->
          map (, constraintID) (queryDependencies enforcer)) $
        indexedConstraints

      varInitialVal =
        fromMaybe (problemDefaultVal problem) .
        (`Map.lookup` (Map.fromList $ problemInitialVals problem))

      allVars = Set.fromList (Map.keys allVarConstraints ++ map fst (problemInitialVals problem))

    lookupNode <- fmap assocsToLookupFunc $
      forM (Set.toList allVars) $ \var -> do
        valRef <- lift $ newSTRef $ varInitialVal var
        versionRef <- lift $ newSTRef $ Version 0
        let
          constraints = fromMaybe [] $ Map.lookup var allVarConstraints
          node = Node
            { nodeVal = valRef
            , nodeVersion = versionRef
            , nodeConstraints = Set.fromList constraints
            }
        return (var, node)

    -- initially, all constraints are dirty
    dirtyConstraints <- lift $ newSTRef $ Set.fromAscList $ map fst indexedConstraints

    let
      applyConstraint :: ConstraintID -> Op s err ()
      applyConstraint constraintID = do
        let constraint = lookupConstraint constraintID
        lookupForQuery <- fmap assocsToLookupFunc $
          forM (queryDependencies (constraintEnforcer constraint)) $ \dep -> do
            lastSeenVersion <- lift $ readSTRef $ constraintLastSeenVersions constraint dep
            let node = lookupNode dep
            currentVersion <- lift $ readSTRef $ nodeVersion node
            currentVal <- lift $ readSTRef $ nodeVal node
            let
              status = case compare lastSeenVersion currentVersion of
                LT -> Changed
                EQ -> Unchanged
                GT -> error $ concat
                  ["Internal error within propagation logic: "
                  ,"last seen version is later than most recent version"]
            return (dep, (currentVal, status))
        updates <- liftEither $ queryCompute (constraintEnforcer constraint) lookupForQuery
        forM_ updates $ \(var, newVal) -> do
          let node = lookupNode var
          oldVal <- lift $ readSTRef $ nodeVal node
          if oldVal /= newVal
          then do
            lift $ modifySTRef' (nodeVersion node) succ
            lift $ writeSTRef (nodeVal node) newVal
            let newDirtyConstraints = Set.delete constraintID $ nodeConstraints node
            lift $ modifySTRef' dirtyConstraints (`Set.union` newDirtyConstraints)
          else
            return ()

    whileM (fmap (not . Set.null) $ lift $ readSTRef dirtyConstraints) $ do
      nextConstraint <- fmap Set.findMin $ lift $ readSTRef dirtyConstraints
      lift $ modifySTRef' dirtyConstraints (Set.delete nextConstraint)
      applyConstraint nextConstraint

    finalVals <- fmap Map.fromList $
      forM (Set.toList allVars) $ \var -> do
        val <- lift $ readSTRef $ nodeVal $ lookupNode var
        return (var, val)

    return (`Map.lookup` finalVals)
