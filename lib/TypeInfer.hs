{-# LANGUAGE ScopedTypeVariables #-}

module TypeInfer where

import qualified Propagate as Prop
import Propagate (queryVar, ChangeStatus(..))

import Unify
import OrderedPair

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Monad (foldM, join)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (first, second)

data Relation = Equality | Inequality Inequality deriving (Eq, Ord, Show)

data Formulation = AppOf | TupleOf deriving (Eq, Ord, Show)

data Constraint var atom
  = BoundConstraint var (Type atom)
  | RelationConstraint var Relation var
  | FormulationConstraint var Formulation var var
  | FuncConstraint var (var, var)
  deriving (Eq, Ord, Show)

data InferenceError var err atom
  = InferenceError
    { errorConstraint :: Constraint var atom
    , errorContent :: TypeError err atom
    }
  | FormMismatch var Formulation (Maybe (Type atom))
  | NotFunction var (Maybe (Type atom))
  deriving (Eq, Ord, Show)

data Problem var err atom = Problem
  { problemConstraints :: [Constraint var atom]
  , problemAtomUnifier :: Unifier err atom
  }

type Solution var err atom = Either (InferenceError var err atom) (var -> Maybe (Type atom))

data ConsolidatedConstraints var err atom = ConsolidatedConstraints
  { boundConstraints :: Map var (Type atom)
  , relationConstraints :: Map (OrderedPair var) Relation
  , formulationConstraints :: [(var, Formulation, var, var)]
  , funcConstraints :: [(var, (var, var))]
  }

emptyConstraints :: ConsolidatedConstraints var err atom
emptyConstraints = ConsolidatedConstraints Map.empty Map.empty [] []

relationConjunction :: Relation -> Relation -> Relation
relationConjunction r1 r2 =
  if r1 == r2
    then r1
    else Equality

flipRelation :: Relation -> Relation
flipRelation Equality = Equality
flipRelation (Inequality LTE) = Inequality GTE
flipRelation (Inequality GTE) = Inequality LTE

splitFormulation :: Formulation -> Maybe (Type atom) -> Maybe (Maybe (Type atom), Maybe (Type atom))

splitFormulation AppOf (Just (App appHead param)) = Just (appHead, param)
splitFormulation AppOf (Just Never) = Just (Just Never, Nothing)

splitFormulation TupleOf (Just (Tuple _ tupleFst tupleSnd)) = Just (tupleFst, tupleSnd)
splitFormulation TupleOf (Just Never) = Just (Nothing, Nothing)

splitFormulation _ (Just _) = Nothing
splitFormulation _ Nothing = Just (Nothing, Nothing)

joinFormulation :: Formulation -> (Maybe (Type atom), Maybe (Type atom)) -> Maybe (Type atom)
joinFormulation AppOf = Just . uncurry App
joinFormulation TupleOf = Just . uncurry (Tuple (SpecialBounds True True))

funcComponents :: Maybe (Type atom) -> Maybe (SpecialBounds, Maybe (Type atom), Maybe (Type atom))
funcComponents Nothing = Just (SpecialBounds False False, Nothing, Nothing)
funcComponents (Just (Func sBounds arg ret)) = Just (sBounds, arg, ret)
funcComponents (Just _) = Nothing

markError :: Constraint var atom -> Either (TypeError err atom) a -> Either (InferenceError var err atom) a
markError constraint = first (InferenceError constraint)

insertMaybe :: (Ord k) => k -> Maybe a -> Map k a -> Map k a
insertMaybe k (Just val) = Map.insert k val
insertMaybe k Nothing = Map.delete k

solve :: forall var err atom. (Ord var, Eq atom) => Problem var err atom -> Solution var err atom
solve problem = do
  let
    unifier = liftAtomUnifier $ problemAtomUnifier problem

    includeConstraint ::
      ConsolidatedConstraints var err atom ->
      Constraint var atom ->
      Either
        (InferenceError var err atom)
        (ConsolidatedConstraints var err atom)

    includeConstraint constraints c@(BoundConstraint var bound) = do
      let oldBoundConstraints = boundConstraints constraints
      let oldBound = Map.lookup var oldBoundConstraints
      newBound <- markError c $ unifyEQ unifier (Just bound) oldBound
      let newBoundConstraints = insertMaybe var newBound oldBoundConstraints
      return constraints { boundConstraints = newBoundConstraints }

    includeConstraint constraints (RelationConstraint var1 rel var2) =
      let
        oldRelations = relationConstraints constraints
        (ordered, flipped) = orderedPair' var1 var2
        normalizedRel = case flipped of
          DidNotFlip -> rel
          DidFlip -> flipRelation rel
        oldRelation = Map.lookup ordered oldRelations
        newRelation = fromMaybe normalizedRel $ fmap (relationConjunction normalizedRel) oldRelation
        newRelations = Map.insert ordered newRelation oldRelations
      in
        Right constraints { relationConstraints = newRelations }

    includeConstraint constraints (FormulationConstraint var1 form var2 var3) =
      let
        oldFormulations = formulationConstraints constraints
        newFormulations = (var1, form, var2, var3) : oldFormulations
      in
        Right constraints { formulationConstraints = newFormulations }

    includeConstraint constraints (FuncConstraint var1 (var2, var3)) =
      let
        oldFuncConstraints = funcConstraints constraints
        newFuncConstraints = (var1, (var2, var3)) : oldFuncConstraints
      in
        Right constraints { funcConstraints = newFuncConstraints }

    enforceRelation ::
      (var, var) ->
      Relation ->
      Prop.ConstraintEnforcer
        var
        (Maybe (Type atom))
        (InferenceError var err atom)

    enforceRelation (var1, var2) Equality =
      go <$> queryVar var1 <*> queryVar var2 where
        go (_, Unchanged) (_, Unchanged) = Right []
        go (bound1, Changed) (_, Unchanged) = Right [(var2, bound1)]
        go (_, Unchanged) (bound2, Changed) = Right [(var1, bound2)]
        go (bound1, Changed) (bound2, Changed) = do
          bound <- markError (RelationConstraint var1 Equality var2) $
            unifyEQ unifier bound1 bound2
          return [(var1, bound), (var2, bound)]

    enforceRelation (var1, var2) (Inequality GTE) = enforceRelation (var2, var1) (Inequality LTE)

    enforceRelation (var1, var2) (Inequality LTE) =
      go <$> queryVar var1 <*> queryVar var2 where
        constraint = RelationConstraint var1 (Inequality LTE) var2
        go (_, Unchanged) (_, Unchanged) = Right []
        go (bound1, Changed) (bound2, Unchanged) = do
            bound2' <- markError constraint $ unifyAsym unifier LTE bound1 bound2
            return [(var2, bound2')]
        go (bound1, Unchanged) (bound2, Changed) = do
          bound1' <- markError constraint $ unifyAsym unifier GTE bound2 bound1
          return [(var1, bound1')]
        go (bound1, Changed) (bound2, Changed) = do
          (bound1', bound2') <- markError constraint $ unifyLTE unifier bound1 bound2
          return [(var1, bound1'), (var2, bound2')]

    enforceEQ ::
      (Maybe (Type atom), ChangeStatus) ->
      (Maybe (Type atom), ChangeStatus) ->
      Either (TypeError err atom) (Maybe (Type atom), Maybe (Type atom))

    enforceEQ (bound1, Unchanged) (bound2, Unchanged) = Right (bound1, bound2)
    enforceEQ (bound1, Changed) (_, Unchanged) = Right (bound1, bound1)
    enforceEQ (_, Unchanged) (bound2, Changed) = Right (bound2, bound2)
    enforceEQ (bound1, Changed) (bound2, Changed) =
      let result = unifyEQ unifier bound1 bound2
      in fmap (\x -> (x, x)) result

    enforceFormulation ::
      (var, Formulation, var, var) ->
      Prop.ConstraintEnforcer
        var
        (Maybe (Type atom))
        (InferenceError var err atom)

    enforceFormulation (wholeVar, form, var1, var2) =
      go <$> queryVar wholeVar <*> queryVar var1 <*> queryVar var2 where
        constraint = FormulationConstraint wholeVar form var1 var2
        go (_, Unchanged) (_, Unchanged) (_, Unchanged) = Right []
        go (wholeBound, wholeChange) (bound1, change1) (bound2, change2) = do
          (part1, part2) <- case splitFormulation form wholeBound of
            Just parts -> Right parts
            Nothing -> Left $ FormMismatch wholeVar form wholeBound
          (part1', bound1') <- markError constraint $
            enforceEQ (part1, wholeChange) (bound1, change1)
          (part2', bound2') <- markError constraint $
            enforceEQ (part2, wholeChange) (bound2, change2)
          let
            wholeUpdate =
              if change1 == Changed || change2 == Changed
                then [(wholeVar, joinFormulation form (part1', part2'))]
                else []
            boundUpdates =
              if wholeChange == Changed
                then [(var1, bound1'), (var2, bound2')]
                else []
          return $ wholeUpdate ++ boundUpdates

    -- Currently largely redundant with formulation constraints, but will need to be treated
    -- separately when interactions are implemented, so we may as well just separate it now.
    enforceFuncConstraint ::
      (var, (var, var)) ->
      Prop.ConstraintEnforcer
        var
        (Maybe (Type atom))
        (InferenceError var err atom)

    enforceFuncConstraint (funcVar, (argVar, retVar)) =
      go <$> queryVar funcVar <*> queryVar argVar <*> queryVar retVar where
        constraint = FuncConstraint funcVar (argVar, retVar)
        go (_, Unchanged) (_, Unchanged) (_, Unchanged) = Right []
        go (funcBound, funcChange) (bound1, change1) (bound2, change2) = do
          (_, part1, part2) <- case funcComponents funcBound of
            Just components -> Right components
            Nothing -> Left $ NotFunction funcVar funcBound
          (part1', bound1') <- markError constraint $
            enforceEQ (part1, funcChange) (bound1, change1)
          (part2', bound2') <- markError constraint $
            enforceEQ (part2, funcChange) (bound2, change2)
          let
            newFunc = Just $ Func (SpecialBounds True True) part1' part2'
            funcUpdate =
              if change1 == Changed || change2 == Changed
                then [(funcVar, newFunc)]
                else []
            boundUpdates =
              if funcChange == Changed
                then [(argVar, bound1'), (retVar, bound2')]
                else []
          return $ funcUpdate ++ boundUpdates

  allConstraints <- foldM includeConstraint emptyConstraints (problemConstraints problem)

  let
    relationEnforcers =
      map (\(vars, rel) -> enforceRelation (items vars) rel) $
      Map.toList $ relationConstraints allConstraints

    formulationEnforcers = map enforceFormulation $ formulationConstraints allConstraints

    funcEnforcers = map enforceFuncConstraint $ funcConstraints allConstraints

    allEnforcers = concat
      [ relationEnforcers
      , formulationEnforcers
      , funcEnforcers
      -- don't forget to add new enforcer types here!
      ]

    propagationProblem = Prop.Problem
      { Prop.problemInitialVals = map (second Just) $ Map.toList $ boundConstraints allConstraints
      , Prop.problemDefaultVal = Nothing
      , Prop.problemConstraints = allEnforcers
      }

  solution <- Prop.solve propagationProblem
  return (join . solution)
