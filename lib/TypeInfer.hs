{-# LANGUAGE ScopedTypeVariables #-}

module TypeInfer where

import qualified Propagate as Prop
import Propagate (queryVar, ChangeStatus(..))

import Unify
import OrderedPair
import DirectedGraph
import TopoSort

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Monad (foldM, join, when)
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
  | RecursiveType -- This should probably have some useful data attached to it
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

data StructuralSizeRelation var = var `StructurallyLargerThan` var

structuralSizeRelations :: Constraint var atom -> [StructuralSizeRelation var]
structuralSizeRelations (BoundConstraint _ _) = []
structuralSizeRelations (RelationConstraint _ _ _) = []
structuralSizeRelations (FormulationConstraint whole _ part1 part2) =
  [ whole `StructurallyLargerThan` part1
  , whole `StructurallyLargerThan` part2
  ]
structuralSizeRelations (FuncConstraint func (arg, ret)) =
  [ func `StructurallyLargerThan` arg
  , func `StructurallyLargerThan` ret
  ]

impliesIllegalRecursiveTypes :: (Ord var) => [Constraint var atom] -> Bool
impliesIllegalRecursiveTypes constraints =
  let
    structuralRelations = concatMap structuralSizeRelations constraints
    consolidatedStructuralRelations =
      outgoingEdges $
      buildDirectedGraph $
      map
        (\(a `StructurallyLargerThan` b) -> a `EdgeTo` b)
        structuralRelations
  in case topoSort consolidatedStructuralRelations of
    Just _ -> False
    Nothing -> True

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
  when (impliesIllegalRecursiveTypes (problemConstraints problem)) $ Left RecursiveType

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
      Either (TypeError err atom) (Maybe (Type atom))

    enforceEQ (bound, Unchanged) (_, Unchanged) = Right bound
    enforceEQ (bound1, Changed) (_, Unchanged) = Right bound1
    enforceEQ (_, Unchanged) (bound2, Changed) = Right bound2
    enforceEQ (bound1, Changed) (bound2, Changed) = unifyEQ unifier bound1 bound2

    enforceFormulation ::
      (var, Formulation, var, var) ->
      Prop.ConstraintEnforcer
        var
        (Maybe (Type atom))
        (InferenceError var err atom)

    enforceFormulation (wholeVar, form, var1, var2) =
      {- Special handling is required when var1 == var2.  See the comments on enforceFuncConstraint
      for more information and explanation.
      -}
      if var1 /= var2
        then let
          constraint = FormulationConstraint wholeVar form var1 var2
          go (_, Unchanged) (_, Unchanged) (_, Unchanged) = Right []
          go (wholeBound, wholeChange) (bound1, change1) (bound2, change2) = do
            (part1, part2) <- case splitFormulation form wholeBound of
              Just parts -> Right parts
              Nothing -> Left $ FormMismatch wholeVar form wholeBound
            part1' <- markError constraint $
              enforceEQ (part1, wholeChange) (bound1, change1)
            part2' <- markError constraint $
              enforceEQ (part2, wholeChange) (bound2, change2)
            let
              wholeUpdate =
                if change1 == Changed || change2 == Changed
                  then [(wholeVar, joinFormulation form (part1', part2'))]
                  else []
              boundUpdates =
                if wholeChange == Changed
                  then [(var1, part1'), (var2, part2')]
                  else []
            return $ wholeUpdate ++ boundUpdates
          in
            go <$> queryVar wholeVar <*> queryVar var1 <*> queryVar var2
        else let
          constraint = FormulationConstraint wholeVar form var1 var2
          go (_, Unchanged) (_, Unchanged) = Right []
          go (wholeBound, _) (bound, _) = do
            -- Here we ignore change information.  See enforceFuncConstraint for more details.
            (part1, part2) <- case splitFormulation form wholeBound of
              Just parts -> Right parts
              Nothing -> Left $ FormMismatch wholeVar form wholeBound
            bound'1 <- markError constraint $ unifyEQ unifier part1 bound
            bound'2 <- markError constraint $ unifyEQ unifier part2 bound
            bound' <- markError constraint $ unifyEQ unifier bound'1 bound'2
            let newWhole = joinFormulation form (bound', bound')
            return [(wholeVar, newWhole), (var1, bound')]
          in
            go <$> queryVar wholeVar <*> queryVar var1

    -- Currently largely redundant with formulation constraints, but will need to be treated
    -- separately when interactions are implemented, so we may as well just separate it now.
    enforceFuncConstraint ::
      (var, (var, var)) ->
      Prop.ConstraintEnforcer
        var
        (Maybe (Type atom))
        (InferenceError var err atom)

    enforceFuncConstraint (funcVar, (argVar, retVar)) =
      {-
      NOTE: Special handling is required to handle the case where argVar == retVar. Ignoring the
      need for this special handling results in a subtle bug in which the new value for argVar is
      overwritten with the new value for retVar (which may be different, since the local unifier
      doesn't know that these type variables happen to be the same).  Because this is a raw
      overwrite, not an equality unification step, information from the argVar unification step can
      be erased.

      TODO: There should be some way to prevent such bugs from being introduced in the future;
      perhaps a runtime warning in Propagate if it sees two simultaneous overwrites of the same
      variable?
      -}
      if argVar /= retVar
        then let
            constraint = FuncConstraint funcVar (argVar, retVar)
            go (_, Unchanged) (_, Unchanged) (_, Unchanged) = Right []
            go (funcBound, funcChange) (bound1, change1) (bound2, change2) = do
              (_, part1, part2) <- case funcComponents funcBound of
                Just components -> Right components
                Nothing -> Left $ NotFunction funcVar funcBound
              part1' <- markError constraint $
                enforceEQ (part1, funcChange) (bound1, change1)
              part2' <- markError constraint $
                enforceEQ (part2, funcChange) (bound2, change2)
              let
                newFunc = Just $ Func (SpecialBounds True True) part1' part2'
                funcUpdate =
                  if change1 == Changed || change2 == Changed
                    then [(funcVar, newFunc)]
                    else []
                boundUpdates =
                  if funcChange == Changed
                    then [(argVar, part1'), (retVar, part2')]
                    else []
              return $ funcUpdate ++ boundUpdates
          in
            go <$> queryVar funcVar <*> queryVar argVar <*> queryVar retVar
        else let
            constraint = FuncConstraint funcVar (argVar, retVar)
            go (_, Unchanged) (_, Unchanged) = Right []
            go (funcBound, _) (bound, _) = do
              {- Here we conservatively ignore any change information. In theory we could get
              performance gains by exploiting this information, as we do in most enforcers. However,
              because of the three-way equality between the two parts of the function bound and the
              one arg/ret bound, and the inability to know which part of the function bounds
              changed, using change information here is somewhat annoying, and the benefits are
              smaller than usual.  Moreover, the special case of `f = a -> a` doesn't come up that
              often.  We should probably fix this branch to use change information at some point,
              but it's a low-priority nice-to-have.
              -}
              (_, part1, part2) <- case funcComponents funcBound of
                Just components -> Right components
                Nothing -> Left $ NotFunction funcVar funcBound
              bound'1 <- markError constraint $ unifyEQ unifier part1 bound
              bound'2 <- markError constraint $ unifyEQ unifier part2 bound
              bound' <- markError constraint $ unifyEQ unifier bound'1 bound'2
              let newFunc = Just $ Func (SpecialBounds True True) bound' bound'
              return [(funcVar, newFunc), (argVar, bound')]
          in
            go <$> queryVar funcVar <*> queryVar argVar

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
