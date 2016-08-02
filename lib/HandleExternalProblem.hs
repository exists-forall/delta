{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HandleExternalProblem (solveExternal) where

import qualified ExternalProblem as External

import PosetSubsetSession
import TypeInfer
import qualified Unify (TypeError (AtomError))
import Unify hiding (AtomError)

import qualified ComplementSet as CSet
import ComplementSet (ComplementSet (..))

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Text as T
import Data.Text (Text)

import Data.Maybe (fromMaybe)
import Control.Monad.State (State, evalState, get, modify')
import Data.Monoid ((<>))
import Data.Void (Void, absurd)

type AtomIdent = External.AtomIdent
type InterIdent = External.InteractionIdent

data AtomBound s = AtomBound
  { boundLo :: Subset 'Upper s AtomIdent -- lower bound
  , boundHi :: Subset 'Lower s AtomIdent -- upper bound
  }
  deriving (Eq, Ord)

data AtomError = AtomError Void -- currently, does not throw errors during unification

-- It might seem weird that the *lower* bound is represented as an *upper* set, and vice-versa.
-- However, it is correct by the following reasoning:
-- If a type `t` satisfies theπ: lower bound, and `t ≤ u`, then `u` also satisfies the lower bound.
-- Therefore, the set of all types satisfying the lower bound forms an upper set.
-- Similar reasoning applies for the upper bound.

defaultLo :: Subset 'Upper s AtomIdent
defaultLo = extendWithParents botNode

defaultHi :: Subset 'Lower s AtomIdent
defaultHi = extendWithChildren topNode

defaultBound :: AtomBound s
defaultBound = AtomBound defaultLo defaultHi

atomUnifyEQ :: EQUnifier AtomError (AtomBound s)
atomUnifyEQ bound1 bound2 =
  let
    (AtomBound lo1 hi1) = fromMaybe defaultBound bound1
    (AtomBound lo2 hi2) = fromMaybe defaultBound bound2
    lo' = intersection lo1 lo2
    hi' = intersection hi1 hi2
  in
    Right $ Just $ AtomBound lo' hi'

atomUnifyLTE :: LTEUnifier AtomError (AtomBound s)
atomUnifyLTE bound1 bound2 =
  let
    (AtomBound lo1 hi1) = fromMaybe defaultBound bound1
    (AtomBound lo2 hi2) = fromMaybe defaultBound bound2
    hi1' = intersection hi1 hi2
    lo2' = intersection lo1 lo2
  in
    Right (Just (AtomBound lo1 hi1'), Just (AtomBound lo2' hi2))

atomUnifyAsym :: AsymUnifier AtomError (AtomBound s)

atomUnifyAsym LTE bound1 bound2 =
  let
    (AtomBound lo1 _) = fromMaybe defaultBound bound1
    (AtomBound lo2 hi2) = fromMaybe defaultBound bound2
    lo2' = intersection lo1 lo2
  in
    Right $ Just $ AtomBound lo2' hi2

atomUnifyAsym GTE bound1 bound2 =
  let
    (AtomBound _ hi1) = fromMaybe defaultBound bound1
    (AtomBound lo2 hi2) = fromMaybe defaultBound bound2
    hi2' = intersection hi1 hi2
  in
    Right $ Just $ AtomBound lo2 hi2'

atomHasLowerBound :: Maybe (AtomBound s) -> Bool
atomHasLowerBound Nothing = False
atomHasLowerBound (Just (AtomBound lo _)) = lo /= extendWithParents botNode

atomUnifier :: Unifier AtomError (AtomBound s)
atomUnifier = Unifier
  { unifyEQ = atomUnifyEQ
  , unifyLTE = atomUnifyLTE
  , unifyAsym = atomUnifyAsym
  , hasLowerBound = atomHasLowerBound
  }

data ProblemContext s = ProblemContext
  { ctxToNode :: AtomIdent -> Node s AtomIdent
  }

newtype InternalVarID = InternalVarID Int deriving (Eq, Ord, Enum)

data TypeVar
  = ExternalVar External.TypeVar
  | QuantifiedVar External.TypeVar External.BoundVar
  | InternalVar External.TypeVar InternalVarID
  deriving (Eq, Ord)

convertConstraint :: forall s.
  ProblemContext s ->
  External.Constraint ->
  [Constraint TypeVar (AtomBound s) InterIdent]

convertConstraint ctx (External.InstantiationConstraint externVar polyType) =
  let
    mintVar :: State InternalVarID TypeVar
    mintVar = do
      varID <- get
      modify' succ
      return $ InternalVar externVar varID

    instantiate ::
      External.PolyType ->
      State InternalVarID ([Constraint TypeVar (AtomBound s) InterIdent], TypeVar)

    instantiate (External.AtomType atom) = do
      let node = ctxToNode ctx atom
      let lo = extendWithParents node
      let hi = extendWithChildren node
      let bound = Atom (AtomBound lo hi)
      var <- mintVar
      return ([BoundConstraint var bound], var)

    instantiate (External.QuantifiedVariable qVar) =
      return ([], QuantifiedVar externVar qVar)

    instantiate (External.TypeApplication headType paramType) = do
      (headConstraints, headVar) <- instantiate headType
      (paramConstraints, paramVar) <- instantiate paramType
      var <- mintVar
      let constraint = FormulationConstraint var AppOf headVar paramVar
      return (constraint : (headConstraints ++ paramConstraints), var)

    instantiate (External.FunctionType argType retType) = do
      (argConstraints, argVar) <- instantiate argType
      (retConstraints, retVar) <- instantiate retType
      var <- mintVar
      let constraint = FuncConstraint var (argVar, retVar)
      return (constraint : (argConstraints ++ retConstraints), var)

    instantiate (External.TupleType fstType sndType) = do
      (fstConstraints, fstVar) <- instantiate fstType
      (sndConstraints, sndVar) <- instantiate sndType
      var <- mintVar
      let constraint = FormulationConstraint var TupleOf fstVar sndVar
      return (constraint : (fstConstraints ++ sndConstraints), var)

    instantiate (External.InteractionType known rest) = do
      knownInstantiations <- mapM (mapM (mapM instantiate)) known -- uses evil tuple Foldable and Functor instances!
      let subConstraints = concatMap (concatMap fst . snd) knownInstantiations
      var <- mintVar
      let
        interConstraints =
          map
            (\(inter, paramInstantiations) ->
              let paramVars = map snd paramInstantiations
              in InteractionConstraint var inter paramVars)
            knownInstantiations

        inters = Set.fromList (map fst known)

        extraConstraints = case rest of
          Just restVar -> [InteractionDifferenceConstraint var inters (QuantifiedVar externVar restVar)]
          Nothing -> [BoundConstraint var (Interaction Map.empty (Included inters))]

      return (extraConstraints ++ interConstraints ++ subConstraints, var)

    instantiate External.NeverType = do
      var <- mintVar
      return ([BoundConstraint var Never], var)

    (internalConstraints, internalRootVar) = evalState (instantiate polyType) (InternalVarID 0)
    rootVar = ExternalVar externVar
    rootConstraint = RelationConstraint rootVar Equality internalRootVar
  in
    rootConstraint : internalConstraints

convertConstraint _ (External.SubtypeConstraint externVar1 externVar2) =
  [RelationConstraint (ExternalVar externVar1) (Inequality LTE) (ExternalVar externVar2)]

convertConstraint _ (External.ExactEqualityConstraint externVar1 externVar2) =
  [RelationConstraint (ExternalVar externVar1) Equality (ExternalVar externVar2)]

convertConstraint _ (External.TypeApplicationEqualityConstraint appliedVar headVar paramVar) =
  [FormulationConstraint (ExternalVar appliedVar) AppOf (ExternalVar headVar) (ExternalVar paramVar)]

convertConstraint _ (External.FunctionEqualityConstraint funcVar argVar retVar) =
  [FuncConstraint (ExternalVar funcVar) (ExternalVar argVar, ExternalVar retVar)]

convertConstraint _ (External.TupleEqualityConstraint tupleVar fstVar sndVar) =
  [FormulationConstraint (ExternalVar tupleVar) TupleOf (ExternalVar fstVar) (ExternalVar sndVar)]

convertConstraint _ (External.InteractionEqualityConstraint var inters rest) =
  let
    interConstraints =
      map
        (\(inter, paramVars) ->
          InteractionConstraint (ExternalVar var) inter (map ExternalVar paramVars))
        inters

    intersSet = Set.fromList (map fst inters)

    extraConstraints = case rest of
      Just restVar -> [InteractionDifferenceConstraint (ExternalVar var) intersSet (ExternalVar restVar)]
      Nothing -> [BoundConstraint (ExternalVar var) (Interaction Map.empty (Included intersSet))]
  in
    interConstraints ++ extraConstraints

constraintExternalVars :: External.Constraint -> [External.TypeVar]
constraintExternalVars (External.InstantiationConstraint var _) = [var]
constraintExternalVars (External.SubtypeConstraint var1 var2) = [var1, var2]
constraintExternalVars (External.ExactEqualityConstraint var1 var2) = [var1, var2]
constraintExternalVars (External.TypeApplicationEqualityConstraint var1 var2 var3) = [var1, var2, var3]
constraintExternalVars (External.FunctionEqualityConstraint var1 var2 var3) = [var1, var2, var3]
constraintExternalVars (External.TupleEqualityConstraint var1 var2 var3) = [var1, var2, var3]
constraintExternalVars (External.InteractionEqualityConstraint var inters (Just restVar)) = var : restVar : concatMap snd inters
constraintExternalVars (External.InteractionEqualityConstraint var inters Nothing) = var : concatMap snd inters

externalVarOf :: TypeVar -> External.TypeVar
externalVarOf (ExternalVar var) = var
externalVarOf (InternalVar var _) = var
externalVarOf (QuantifiedVar var _) = var

relevantVars :: Constraint TypeVar (AtomBound s) InterIdent -> [External.TypeVar]
relevantVars (BoundConstraint var _) = [externalVarOf var]
relevantVars (RelationConstraint var1 _ var2) = map externalVarOf [var1, var2]
relevantVars (FormulationConstraint var1 _ var2 var3) = map externalVarOf [var1, var2, var3]
relevantVars (FuncConstraint var1 (var2, var3)) = map externalVarOf [var1, var2, var3]
relevantVars (InteractionConstraint var _ paramVars) = map externalVarOf (var : paramVars)
relevantVars (InteractionDifferenceConstraint var _ restVar) = map externalVarOf [var, restVar]

formatAtomIdent :: AtomIdent -> Text
formatAtomIdent (External.AtomIdent []) = "{empty path}"
formatAtomIdent (External.AtomIdent path) = last path

formatInterIdent :: InterIdent -> Text
formatInterIdent (External.InteractionIdent []) = "{empty path}"
formatInterIdent (External.InteractionIdent path) = last path

formatNode :: Node s AtomIdent -> Text
formatNode node =
  case fromNode node of
    NodeValue ident -> formatAtomIdent ident
    TopNodeValue -> "{the supertype of all types}"
    BotNodeValue -> "Never"

formatType :: Maybe (Type (AtomBound s) InterIdent) -> Text

formatType (Just (Atom (AtomBound lo hi))) =
  case filter (`member` hi) (minimal lo) of
    [] -> "{no possible type}"
    [node] -> formatNode node
    nodes -> "{any of these types: " <> T.intercalate " or " (map formatNode nodes) <> "}"

formatType (Just (App headType paramType)) =
  formatType headType <> "<" <> formatType paramType <> ">"

formatType (Just (Func _ argType retType)) =
  formatType argType <> "->" <> formatType retType

formatType (Just (Tuple _ fstType sndType)) =
  formatType fstType <> "," <> formatType sndType

formatType (Just (Interaction lo _)) =
  if Map.null lo
    then "Pure"
    else
      T.intercalate " | " $
        map
          (\(inter, params) ->
            let formattedParams = map (\param -> "<" <> formatType param <> ">") params
            in formatInterIdent inter <> mconcat formattedParams)
          (Map.toList lo)

formatType (Just Never) = "Never"

formatType Nothing = "{unconstrainted type}"

formatTypeError :: TypeError AtomError (AtomBound s) InterIdent -> Text

formatTypeError (Unify.AtomError (AtomError void)) = absurd void

formatTypeError (StructureMismatch t1 t2) =
  "I couldn't match type `" <> formatType (Just t1) <> "` with type `" <> formatType (Just t2) <> "`, " <>
    "because they are not structurally compatible."

formatTypeError (NotNeverConvertible t1) =
  "Type `" <> formatType (Just t1) <> "` is not convertible to `Never`"

formatTypeError (InteractionArityMismatch params1 params2) =
    -- TODO: This definitely needs to be more helpful!  It is very hard for a user to localize this error
    -- It's a rare enough error, though, that making it more helpful isn't the highest priority.
    "I got a conflicting number of arguments for the parameters on two related interactions.\n" <>
    "One seems to take the arguments:\n\t" <> T.intercalate ", " (map formatType params1) <> "\n" <>
    "But the other seems to take the arguments:\n\t" <> T.intercalate ", " (map formatType params2)

convertError ::
  InferenceError TypeVar AtomError (AtomBound s) InterIdent ->
  (Text, [External.TypeVar])

convertError (InferenceError constraint content) =
  let
    vars = relevantVars constraint
    formatted = formatTypeError content
  in
    (formatted, vars)

convertError (FormMismatch var form badType) =
  let
    formDescription = case form of
      AppOf -> "type application (i.e., a type of the form `foo<bar>`)"
      TupleOf -> "tuple (i.e., a type of the form `foo,bar`)"
    msg =
      "I expected the type here to be a " <> formDescription <> ", but instead I inferred this type: " <>
      formatType badType
  in
    (msg, [externalVarOf var])

convertError (NotFunction var badType) =
  let
    msg =
      "I expected the type here to be a function (i.e. a type of the form `foo->bar`), but instead " <>
      "I inferred this type: " <>
      formatType badType
  in
    (msg, [externalVarOf var])

convertError RecursiveType =
  ("I'm inferring some weird, infinitely-deep types.  This is usually a problem with recursion.", [])

convertError (NotInteraction var badType) =
  let
    msg =
      "I expected the type here to be an interaction, but instead I found this type: " <>
      formatType badType
  in
    (msg, [externalVarOf var])

convertError (InteractionCantContain var disallowed badType) =
  let
    msg =
      "I expected the interaction type here *not* to contain:\n" <>
      "\t" <> T.intercalate " or " (map formatInterIdent (Set.toList disallowed)) <> "\n" <>
      "But instead I inferred this type: " <> formatType badType
  in
    (msg, [externalVarOf var])

convertSolution :: Maybe (Type (AtomBound s) InterIdent) -> Either Text External.TypeSolution

convertSolution Nothing = Right External.NeverTypeSolution

convertSolution (Just (Atom (AtomBound lo hi))) =
  let
    minAtoms = minimal lo
    maxAtoms = maximal hi
  in case filter (`member` hi) (minimal lo) of
    [] -> Left $
      "I can't find any type that could work here." <>
      "\nI'm inferring that this type must be convertible from:\n    " <>
      T.intercalate " or " (map formatNode minAtoms) <>
      "\nBut also that it must be convertible to:\n    " <>
      T.intercalate " or " (map formatNode maxAtoms)
    [node] ->
      case fromNode node of
        NodeValue atomIdent -> Right $ External.AtomTypeSolution atomIdent
        TopNodeValue -> Left "I'm inferring that all possible types have to be convertible to this type, which is impossible."
        BotNodeValue -> Right External.NeverTypeSolution
    nodes -> Left $
      "There are several possible types that could work here, but I have no way to decide which one to use.\n" <>
      "In other words, the type is ambiguous.\n" <>
      "Here are the most specific types that could work here:\n\t" <>
      T.intercalate " or " (map formatNode nodes)

convertSolution (Just (App headType paramType)) = do
  headSolution <- convertSolution headType
  case headSolution of
    External.NeverTypeSolution -> return External.NeverTypeSolution
    _ -> do
      paramSolution <- convertSolution paramType
      return $ External.TypeApplicationSolution headSolution paramSolution

convertSolution (Just (Func sBounds argType retType)) =
  if constrainedLo sBounds
    then do
      argSolution <- convertSolution argType
      retSolution <- convertSolution retType
      return $ External.FunctionTypeSolution argSolution retSolution
    else
      return External.NeverTypeSolution

convertSolution (Just (Tuple sBounds fstType sndType)) =
  if constrainedLo sBounds
    then do
      fstSolution <- convertSolution fstType
      sndSolution <- convertSolution sndType
      return $ External.TupleTypeSolution fstSolution sndSolution
    else
      return External.NeverTypeSolution

convertSolution t@(Just (Interaction lo hi)) =
  if all (`CSet.member` hi) (Map.keys lo)
    then do
      solutions <- mapM (mapM convertSolution) lo
      return $ External.InteractionTypeSolution (Map.toList solutions)
    else
      let
        contextMsg = case hi of
          Included included ->
            if Set.null included
              then "In a pure context"
              else
                "In a context which only allows:\n\t" <>
                T.intercalate ", " (map formatInterIdent (Set.toList included))
          Excluded excluded ->
            "In a context which does not allow:\n\t" <>
            T.intercalate ", " (map formatInterIdent (Set.toList excluded))
      in Left $
        "I inferred the following interaction:\n\t" <> formatType t <> "\n" <> contextMsg

convertSolution (Just Never) =
  return External.NeverTypeSolution

unwrapExternalVar :: External.TypeVar -> External.UnwrappedTypeVar
unwrapExternalVar (External.TypeVar ident) = ident

solveSubProblem :: ProblemContext s -> [External.Constraint] -> External.SubProblemResult
solveSubProblem ctx externConstraints =
  let
    constraints = concatMap (convertConstraint ctx) externConstraints
    inferenceProblem = Problem
      { problemConstraints = constraints
      , problemAtomUnifier = atomUnifier
      }
    allVars = Set.toList $ Set.fromList $ concatMap constraintExternalVars externConstraints
  in case solve inferenceProblem of
    Left err -> uncurry External.Error (convertError err)
    Right solution ->
      let
        getVar var =
          case convertSolution $ solution $ ExternalVar var of
            Left errMsg -> Left (errMsg, [var])
            Right sol -> Right (unwrapExternalVar var, sol)
      in case mapM getVar allVars of
        Left (msg, vars) -> External.Error msg vars
        Right bindings -> External.Success $ Map.fromList bindings

solveExternal :: External.Problem -> External.Results
solveExternal problem =
  let relations = map (\(External.Conversion sub super) -> sub `ChildOf` super) $ External.conversions problem
  in case buildPoset relations of
    Nothing -> External.FatalError "Circular type conversion relationship."
    Just poset -> runSession go poset where
      go :: forall s.
        (AtomIdent -> Node s AtomIdent) ->
        External.Results
      go toNode =
        let
          ctx = ProblemContext toNode
          subproblems = External.subproblems problem
        in
          External.Results $ Map.map (solveSubProblem ctx) subproblems
