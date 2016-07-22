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

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Text as T
import Data.Text (Text)

import Data.Maybe (fromMaybe)
import Control.Monad.State (State, evalState, get, modify')
import Data.Monoid ((<>))
import Data.Void (Void, absurd)

type AtomIdent = External.AtomIdent

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
  , ctxFromNode :: Node s AtomIdent -> NodeValue AtomIdent
  }

newtype InternalVarID = InternalVarID Int deriving (Eq, Ord, Enum)

data TypeVar
  = ExternalVar External.TypeVar
  | QuantifiedVar External.TypeVar External.BoundVar
  | InternalVar External.TypeVar InternalVarID
  deriving (Eq, Ord)

convertBounds :: ProblemContext s -> External.Bounds -> Type (AtomBound s)

convertBounds ctx (External.AtomTypeBounds externLo externHi) =
  let
    lo = fromMaybe defaultLo $ fmap (extendWithParents . ctxToNode ctx) externLo
    hi = fromMaybe defaultHi $ fmap (extendWithChildren . ctxToNode ctx) externHi
  in
    Atom (AtomBound lo hi)

convertBounds ctx (External.TypeApplicationBounds headBounds tailBounds) =
  App (fmap (convertBounds ctx) headBounds) (fmap (convertBounds ctx) tailBounds)

convertBounds ctx (External.FunctionTypeBounds argBounds retBounds) =
  -- Should the API expose some way to specify the `SpecialBounds`?
  Func (SpecialBounds True True) (fmap (convertBounds ctx) argBounds) (fmap (convertBounds ctx) retBounds)

convertBounds ctx (External.TupleTypeBounds fstBounds sndBounds) =
  Tuple (SpecialBounds True True) (fmap (convertBounds ctx) fstBounds) (fmap (convertBounds ctx) sndBounds)

convertBounds _ External.NeverTypeBounds = Never

convertConstraint :: forall s.
  ProblemContext s ->
  External.Constraint ->
  [Constraint TypeVar (AtomBound s)]

convertConstraint ctx (External.BoundsConstraint externVar externBounds) =
  [BoundConstraint (ExternalVar externVar) (convertBounds ctx externBounds)]

convertConstraint ctx (External.InstantiationConstraint externVar polyType) =
  let
    mintVar :: State InternalVarID TypeVar
    mintVar = do
      varID <- get
      modify' succ
      return $ InternalVar externVar varID

    instantiate ::
      External.PolyType ->
      State InternalVarID ([Constraint TypeVar (AtomBound s)], TypeVar)

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

constraintExternalVars :: External.Constraint -> [External.TypeVar]
constraintExternalVars (External.BoundsConstraint var _) = [var]
constraintExternalVars (External.InstantiationConstraint var _) = [var]
constraintExternalVars (External.SubtypeConstraint var1 var2) = [var1, var2]
constraintExternalVars (External.ExactEqualityConstraint var1 var2) = [var1, var2]
constraintExternalVars (External.TypeApplicationEqualityConstraint var1 var2 var3) = [var1, var2, var3]
constraintExternalVars (External.FunctionEqualityConstraint var1 var2 var3) = [var1, var2, var3]
constraintExternalVars (External.TupleEqualityConstraint var1 var2 var3) = [var1, var2, var3]

externalVarOf :: TypeVar -> External.TypeVar
externalVarOf (ExternalVar var) = var
externalVarOf (InternalVar var _) = var
externalVarOf (QuantifiedVar var _) = var

relevantVars :: Constraint TypeVar (AtomBound s) -> [External.TypeVar]
relevantVars (BoundConstraint var _) = [externalVarOf var]
relevantVars (RelationConstraint var1 _ var2) = map externalVarOf [var1, var2]
relevantVars (FormulationConstraint var1 _ var2 var3) = map externalVarOf [var1, var2, var3]
relevantVars (FuncConstraint var1 (var2, var3)) = map externalVarOf [var1, var2, var3]

formatAtomIdent :: AtomIdent -> Text
formatAtomIdent (External.AtomIdent []) = "{empty path}"
formatAtomIdent (External.AtomIdent path) = last path

formatNode :: ProblemContext s -> Node s AtomIdent -> Text
formatNode ctx node =
  case ctxFromNode ctx node of
    NodeValue ident -> formatAtomIdent ident
    TopNodeValue -> "{the supertype of all types}"
    BotNodeValue -> "Never"

formatType :: ProblemContext s -> Maybe (Type (AtomBound s)) -> Text

formatType ctx (Just (Atom (AtomBound lo hi))) =
  case filter (`member` hi) (minimal lo) of
    [] -> "{no possible type}"
    [node] -> formatNode ctx node
    nodes -> "{any of these types: " <> T.intercalate " or " (map (formatNode ctx) nodes) <> "}"

formatType ctx (Just (App headType paramType)) =
  formatType ctx headType <> "<" <> formatType ctx paramType <> ">"

formatType ctx (Just (Func _ argType retType)) =
  formatType ctx argType <> "->" <> formatType ctx retType

formatType ctx (Just (Tuple _ fstType sndType)) =
  formatType ctx fstType <> "," <> formatType ctx sndType

formatType _ (Just Never) = "Never"

formatType _ Nothing = "{unconstrainted type}"

formatTypeError :: ProblemContext s -> TypeError AtomError (AtomBound s) -> Text

formatTypeError _ (Unify.AtomError (AtomError void)) = absurd void

formatTypeError ctx (StructureMismatch t1 t2) =
  "I couldn't match type `" <> formatType ctx (Just t1) <> "` with type `" <> formatType ctx (Just t2) <> "`, " <>
    "because they are not structurally compatible."

formatTypeError ctx (NotNeverConvertible t1) =
  "Type `" <> formatType ctx (Just t1) <> "` is not convertible to `Never`"

convertError ::
  ProblemContext s ->
  InferenceError TypeVar AtomError (AtomBound s) ->
  (Text, [External.TypeVar])

convertError ctx (InferenceError constraint content) =
  let
    vars = relevantVars constraint
    formatted = formatTypeError ctx content
  in
    (formatted, vars)

convertError ctx (FormMismatch var form badType) =
  let
    formDescription = case form of
      AppOf -> "type application (i.e., a type of the form `foo<bar>`)"
      TupleOf -> "tuple (i.e., a type of the form `foo,bar`)"
    msg =
      "I expected the type here to be a " <> formDescription <> ", but instead I inferred this type: " <>
      formatType ctx badType
  in
    (msg, [externalVarOf var])

convertError ctx (NotFunction var badType) =
  let
    msg =
      "I expected the type here to be a function (i.e. a type of the form `foo->bar`), but instead " <>
      "I inferred this type: " <>
      formatType ctx badType
  in
    (msg, [externalVarOf var])

convertError _ RecursiveType =
  ("I'm inferring some weird, infinitely-deep types.  This is usually a problem with recursion.", [])

convertSolution :: ProblemContext s -> Maybe (Type (AtomBound s)) -> Either Text External.TypeSolution

convertSolution _ Nothing = Right External.NeverTypeSolution

convertSolution ctx (Just (Atom (AtomBound lo hi))) =
  let
    minAtoms = minimal lo
    maxAtoms = maximal hi
  in case filter (`member` hi) (minimal lo) of
    [] -> Left $
      "I can't find any type that could work here." <>
      "\nI'm inferring that this type must be convertible from:\n    " <>
      T.intercalate " or " (map (formatNode ctx) minAtoms) <>
      "\nBut also that it must be convertible to:\n    " <>
      T.intercalate " or " (map (formatNode ctx) maxAtoms)
    [node] ->
      case ctxFromNode ctx node of
        NodeValue atomIdent -> Right $ External.AtomTypeSolution atomIdent
        TopNodeValue -> Left "I'm inferring that all possible types have to be convertible to this type, which is impossible."
        BotNodeValue -> Right External.NeverTypeSolution
    nodes -> Left $
      "There are several possible types that could work here, but I have no way to decide which one to use.\n" <>
      "In other words, the type is ambiguous.\n" <>
      "Here are the most specific types that could work here:\n\t" <>
      T.intercalate " or " (map (formatNode ctx) nodes)

convertSolution ctx (Just (App headType paramType)) = do
  headSolution <- convertSolution ctx headType
  case headSolution of
    External.NeverTypeSolution -> return External.NeverTypeSolution
    _ -> do
      paramSolution <- convertSolution ctx paramType
      return $ External.TypeApplicationSolution headSolution paramSolution

convertSolution ctx (Just (Func sBounds argType retType)) =
  if constrainedLo sBounds
    then do
      argSolution <- convertSolution ctx argType
      retSolution <- convertSolution ctx retType
      return $ External.FunctionTypeSolution argSolution retSolution
    else
      return External.NeverTypeSolution

convertSolution ctx (Just (Tuple sBounds fstType sndType)) =
  if constrainedLo sBounds
    then do
      fstSolution <- convertSolution ctx fstType
      sndSolution <- convertSolution ctx sndType
      return $ External.TupleTypeSolution fstSolution sndSolution
    else
      return External.NeverTypeSolution

convertSolution _ (Just Never) =
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
    Left err -> uncurry External.Error (convertError ctx err)
    Right solution ->
      let
        getVar var =
          case convertSolution ctx $ solution $ ExternalVar var of
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
        (Node s AtomIdent -> NodeValue AtomIdent) ->
        External.Results
      go toNode fromNode =
        let
          ctx = ProblemContext toNode fromNode
          subproblems = External.subproblems problem
        in
          External.Results $ Map.map (solveSubProblem ctx) subproblems
