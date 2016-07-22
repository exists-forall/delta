{-# LANGUAGE ScopedTypeVariables #-}

module Unify
  ( Type (..)
  , SpecialBounds (..)
  , Inequality (..)
  , TypeError (..)

  , EQUnifier
  , LTEUnifier
  , AsymUnifier
  , Unifier (..)

  , liftAtomUnifier
  )
where

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import Data.Tuple (swap)
import Control.Applicative ((<|>))

import Data.Maybe (fromJust)

data Inequality = LTE | GTE deriving (Eq, Ord, Show)

flipInequality :: Inequality -> Inequality
flipInequality LTE = GTE
flipInequality GTE = LTE

-- Technically, these are "type bounds", not "types", and "atom"s are really "atomBound"s
-- A "Type" conceptually involves both a lower and upper bound, which are both themselves "types"
-- in the conventional sense.
data Type atom inter
  = Atom atom
  | App (Maybe (Type atom inter)) (Maybe (Type atom inter))
  | Func SpecialBounds (Maybe (Type atom inter)) (Maybe (Type atom inter))
  | Tuple SpecialBounds (Maybe (Type atom inter)) (Maybe (Type atom inter))
  | Interaction (Map inter [(Maybe (Type atom inter))]) (Maybe (Set inter))
  | Never
  deriving (Eq, Ord, Show)

{- Conceptually, a function type bound is very much like a type application bound in which the type
constructor is the special (->) atom.  Function type bounds are not coercible to or directly
compatible with type application bounds, but they nonetheless share the property that their type
constructor itself has an upper and lower bound during type inference.  Like all other types, the
conceptual (->) atom is a supertype of the special bot (⊥) type, and a subtype of the special top (⊤)
type.


Therefore, if type variable `t` is a function type bound and conceptually `t = c<a><b>`, there are
actually four subtly distinct possibilities:

1. (->) ≤ c ≤ (->)
2. ⊥ ≤ c ≤ (->)
3. (->) ≤ c ≤ ⊤
4. ⊥ ≤ c ≤ ⊤

While the difference between these scenarios may seem irrelevant, it becomes important when unifying
function types with ⊥ (the bottom type, written `Never` in Delta code).  If we have the constraint
`t ≤ ⊥`, scenarios #2 and #4 admit the solution `c = ⊥`, whereas scenarios #1 and #3 admit no
solution. Additionally, while scenario #4 may seem odd in that it does not actually constrain the
type to be a function at all (although it does forbid it from being a type application or tuple), it
can be important as an intermediate step during unification.  For example, if `u` is initially
completely unconstrained, and we apply the constraint that `u ≤ t` where the bound of `t` is a
function bound corresponding to scenario #3, then `u` enters scenario #4.

All the above logic also applies to Tuple types and the special (,) pseudo-type-constructor.

The `SpecialBounds` type distinguishes between these four possibilities for functions and tuples.
-}
data SpecialBounds = SpecialBounds
  { constrainedLo :: Bool
  , constrainedHi :: Bool
  }
  deriving (Eq, Ord, Show)

specialBoundsEQ :: SpecialBounds -> SpecialBounds -> SpecialBounds
specialBoundsEQ (SpecialBounds lo1 hi1) (SpecialBounds lo2 hi2) = SpecialBounds (lo1 || lo2) (hi1 || hi2)

specialBoundsLTE :: SpecialBounds -> SpecialBounds -> (SpecialBounds, SpecialBounds)
specialBoundsLTE (SpecialBounds lo1 hi1) (SpecialBounds lo2 hi2) =
  (SpecialBounds lo1 (hi1 || hi2), SpecialBounds (lo1 || lo2) hi2)

specialBoundsAsym :: Inequality -> SpecialBounds -> SpecialBounds -> SpecialBounds
specialBoundsAsym LTE (SpecialBounds lo1 _) (SpecialBounds lo2 hi2) = SpecialBounds (lo1 || lo2) hi2
specialBoundsAsym GTE (SpecialBounds _ hi1) (SpecialBounds lo2 hi2) = SpecialBounds lo2 (hi1 || hi2)

data TypePair atom inter
  = TwoAtoms (Maybe atom) (Maybe atom)
  | TwoApps (Maybe (Type atom inter), Maybe (Type atom inter)) (Maybe (Type atom inter), Maybe (Type atom inter))
  | TwoFuncs
    (SpecialBounds, Maybe (Type atom inter), Maybe (Type atom inter))
    (SpecialBounds, Maybe (Type atom inter), Maybe (Type atom inter))
  | TwoTuples
    (SpecialBounds, Maybe (Type atom inter), Maybe (Type atom inter))
    (SpecialBounds, Maybe (Type atom inter), Maybe (Type atom inter))
  | NeverAndType (Maybe (Type atom inter))
  | TypeAndNever (Maybe (Type atom inter))

-- Unifies two bounds related by an equality constraint.
-- Because the constraint is an equality constraint, unification must result in both bounds
-- being identical, so only a single bound is returned.
-- EQUnifiers make no assumption of prior local consistency between the two bounds.
type EQUnifier err bound = Maybe bound -> Maybe bound -> Either err (Maybe bound)

-- Unifies two bounds related by a less-than-or-equal-to constraint.
-- LTEUnifiers make no assumption of prior local consistency between the two bounds.
type LTEUnifier err bound = Maybe bound -> Maybe bound -> Either err (Maybe bound, Maybe bound)

-- Unifies two bounds related by an ienquality, assuming that the first bound "has nothing to learn"
-- from the second.  This essentially means that the bounds were locally consistent, and then the
-- left one changed.
-- This type of unifier is included purely as a performance optimization.
type AsymUnifier err bound = Inequality -> Maybe bound -> Maybe bound -> Either err (Maybe bound)

data Unifier err bound = Unifier
  { unifyEQ :: EQUnifier err bound
  , unifyLTE :: LTEUnifier err bound
  , unifyAsym :: AsymUnifier err bound
  , hasLowerBound :: Maybe bound -> Bool
  }

data TypeError err atom inter
  = AtomError err
  | StructureMismatch (Type atom inter) (Type atom inter)
  | NotNeverConvertible (Type atom inter)
  deriving (Eq, Ord, Show)

dup :: (a -> a -> b) -> a -> b
dup f x = f x x

mapBoth :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
mapBoth f g (x, y) = (f x, g y)

mapBothSame :: (a -> b) -> (a, a) -> (b, b)
mapBothSame = dup mapBoth

mapEither :: (l -> l') -> (r -> r') -> Either l r -> Either l' r'
mapEither f _ (Left x) = Left (f x)
mapEither _ g (Right y) = Right (g y)

structureUnify ::
  Maybe (Type atom inter) ->
  Maybe (Type atom inter) ->
  Either (TypeError err atom inter) (Maybe (TypePair atom inter))

structureUnify Nothing Nothing = Right Nothing

structureUnify (Just Never) t = Right (Just (NeverAndType t))
structureUnify t (Just Never) = Right (Just (TypeAndNever t))

structureUnify (Just (Atom atom1)) (Just (Atom atom2)) = Right (Just (TwoAtoms (Just atom1) (Just atom2)))
structureUnify (Just (Atom atom1)) Nothing = Right (Just (TwoAtoms (Just atom1) Nothing))
structureUnify Nothing (Just (Atom atom2)) = Right (Just (TwoAtoms Nothing (Just atom2)))

structureUnify (Just (App head1 param1)) (Just (App head2 param2)) = Right (Just (TwoApps (head1, param1) (head2, param2)))
structureUnify (Just (App head1 param1)) Nothing = Right (Just (TwoApps (head1, param1) (Nothing, Nothing)))
structureUnify Nothing (Just (App head2 param2)) = Right (Just (TwoApps (Nothing, Nothing) (head2, param2)))

structureUnify (Just (Func sBounds1 arg1 ret1)) (Just (Func sBounds2 arg2 ret2)) =
  Right (Just (TwoFuncs (sBounds1, arg1, ret1) (sBounds2, arg2, ret2)))

structureUnify (Just (Func sBounds1 arg1 ret1)) Nothing =
  Right (Just (TwoFuncs (sBounds1, arg1, ret1) (SpecialBounds False False, Nothing, Nothing)))

structureUnify Nothing (Just (Func sBounds2 arg2 ret2)) =
  Right (Just (TwoFuncs (SpecialBounds False False, Nothing, Nothing) (sBounds2, arg2, ret2)))

structureUnify (Just (Tuple sBounds1 fst1 snd1)) (Just (Tuple sBounds2 fst2 snd2)) =
  Right (Just (TwoTuples (sBounds1, fst1, snd1) (sBounds2, fst2, snd2)))

structureUnify (Just (Tuple sBounds1 fst1 snd1)) Nothing =
  Right (Just (TwoTuples (sBounds1, fst1, snd1) (SpecialBounds False False, Nothing, Nothing)))

structureUnify Nothing (Just (Tuple sBounds2 fst2 snd2)) =
  Right (Just (TwoTuples (SpecialBounds False False, Nothing, Nothing) (sBounds2, fst2, snd2)))

structureUnify (Just bound1) (Just bound2) = Left (StructureMismatch bound1 bound2)

liftAtomUnifier :: forall err atom inter. (Ord inter) => Unifier err atom -> Unifier (TypeError err atom inter) (Type atom inter)
liftAtomUnifier atomUnifier =
  let
    typeUnifyEQ :: EQUnifier (TypeError err atom inter) (Type atom inter)
    typeUnifyEQ (Just bound1) (Just bound2) =
      case (bound1, bound2) of
        (Atom atom1, Atom atom2) ->
          mapEither AtomError (fmap Atom) (unifyEQ atomUnifier (Just atom1) (Just atom2))

        (App head1 param1, App head2 param2) -> do
          headBound <- typeUnifyEQ head1 head2
          paramBound <- typeUnifyEQ param1 param2
          return (Just (App headBound paramBound))

        (Func sBounds1 arg1 ret1, Func sBounds2 arg2 ret2) -> do
          let sBounds = specialBoundsEQ sBounds1 sBounds2
          arg <- typeUnifyEQ arg1 arg2
          ret <- typeUnifyEQ ret1 ret2
          return (Just (Func sBounds arg ret))

        (Tuple sBounds1 fst1 snd1, Tuple sBounds2 fst2 snd2) -> do
          let sBounds = specialBoundsEQ sBounds1 sBounds2
          fstBound <- typeUnifyEQ fst1 fst2
          sndBound <- typeUnifyEQ snd1 snd2
          return (Just (Tuple sBounds fstBound sndBound))

        (Never, Never) -> Right (Just Never)
        (t, Never) ->
          if lowerBound $ Just t
            then Left $ NotNeverConvertible t
            else Right $ Just Never
        (Never, t) -> typeUnifyEQ (Just t) (Just Never)

        (_, _) -> Left (StructureMismatch bound1 bound2)

    typeUnifyEQ bound1 bound2 = Right (bound1 <|> bound2)

    typeUnifyLTE :: LTEUnifier (TypeError err atom inter) (Type atom inter)
    typeUnifyLTE bound1 bound2 = do
      bounds <- structureUnify bound1 bound2
      case bounds of
        Just (TwoAtoms atom1 atom2) ->
          mapEither AtomError (mapBothSame (fmap Atom)) (unifyLTE atomUnifier atom1 atom2)

        Just (TwoApps (head1, param1) (head2, param2)) -> do
          (head1', head2') <- typeUnifyLTE head1 head2
          (param1', param2') <- fmap (dup (,)) (typeUnifyEQ param1 param2) -- type constructors are invariant in their type parameter
          return (Just (App head1' param1'), Just (App head2' param2'))

        Just (TwoFuncs (sBounds1, arg1, ret1) (sBounds2, arg2, ret2)) -> do
          let (sBounds1', sBounds2') = specialBoundsLTE sBounds1 sBounds2
          (arg1', arg2') <- fmap swap (typeUnifyLTE arg2 arg1) -- functions are contravariant in their argument
          (ret1', ret2') <- typeUnifyLTE ret1 ret2
          return (Just (Func sBounds1' arg1' ret1'), Just (Func sBounds2' arg2' ret2'))

        Just (TwoTuples (sBounds1, fst1, snd1) (sBounds2, fst2, snd2)) -> do
          let (sBounds1', sBounds2') = specialBoundsLTE sBounds1 sBounds2
          (fst1', fst2') <- typeUnifyLTE fst1 fst2
          (snd1', snd2') <- typeUnifyLTE snd1 snd2
          return (Just (Tuple sBounds1' fst1' snd1'), Just (Tuple sBounds2' fst2' snd2'))

        Just (NeverAndType _) -> Right (bound1, bound2) -- `∀ τ. ⊥ ≤ τ`, so do nothing
        Just (TypeAndNever t) ->
          if lowerBound t
            then Left $ NotNeverConvertible $ fromJust t -- lowerBound is False for Nothing
            else Right $ (Just Never, Just Never)

        Nothing -> return (Nothing, Nothing)

    typeUnifyAsym :: AsymUnifier (TypeError err atom inter) (Type atom inter)
    typeUnifyAsym inequality bound1 bound2 = do
      bounds <- structureUnify bound1 bound2
      case bounds of
        Just (TwoAtoms atom1 atom2) ->
          mapEither AtomError (fmap Atom) (unifyAsym atomUnifier inequality atom1 atom2)

        Just (TwoApps (head1, param1) (head2, _)) -> do
          head2' <- typeUnifyAsym inequality head1 head2
          return (Just (App head2' param1))

        Just (TwoFuncs (sBounds1, arg1, ret1) (sBounds2, arg2, ret2)) -> do
          let sBounds2' = specialBoundsAsym inequality sBounds1 sBounds2
          arg2' <- typeUnifyAsym (flipInequality inequality) arg1 arg2
          ret2' <- typeUnifyAsym inequality ret1 ret2
          return (Just (Func sBounds2' arg2' ret2'))

        Just (TwoTuples (sBounds1, fst1, snd1) (sBounds2, fst2, snd2)) -> do
          let sBounds2' = specialBoundsAsym inequality sBounds1 sBounds2
          fst2' <- typeUnifyAsym inequality fst1 fst2
          snd2' <- typeUnifyAsym inequality snd1 snd2
          return (Just (Tuple sBounds2' fst2' snd2'))

        Just (NeverAndType t) ->
          case inequality of
            LTE -> Right t
            GTE -> if lowerBound t
              then Left $ NotNeverConvertible $ fromJust t
              else Right $ Just Never

        Just (TypeAndNever t) ->
          case inequality of
            GTE -> Right t
            LTE -> if lowerBound t
              then Left $ NotNeverConvertible $ fromJust t
              else Right $ Just Never

        Nothing -> Right Nothing

    lowerBound :: Maybe (Type atom inter) -> Bool

    lowerBound Nothing = False

    lowerBound (Just (Atom atom)) =
      hasLowerBound atomUnifier (Just atom)

    lowerBound (Just (App headBound _)) =
      lowerBound headBound

    lowerBound (Just (Func sBounds _ _)) =
      constrainedLo sBounds

    lowerBound (Just (Tuple sBounds _ _)) =
      constrainedLo sBounds

    lowerBound (Just Never) = False
  in
    Unifier
      { unifyEQ = typeUnifyEQ
      , unifyLTE = typeUnifyLTE
      , unifyAsym = typeUnifyAsym
      , hasLowerBound = lowerBound
      }
