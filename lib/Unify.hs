{-# LANGUAGE ScopedTypeVariables #-}

module Unify
  ( Type (..)
  , Inequality (..)
  , TypeError (..)

  , EQUnifier
  , LTEUnifier
  , AsymUnifier
  , Unifier (..)

  , liftAtomUnifier
  )
where

import Data.Tuple (swap)
import Control.Applicative ((<|>))

data Inequality = LTE | GTE deriving (Eq, Ord, Show)

flipInequality :: Inequality -> Inequality
flipInequality LTE = GTE
flipInequality GTE = LTE

-- Technically, these are "type bounds", not "types", and "atom"s are really "atomBound"s
-- A "Type" conceptually involves both a lower and upper bound, which are both themselves "types"
-- in the conventional sense.
data Type atom
  = Atom atom
  | App (Maybe (Type atom)) (Maybe (Type atom))
  | Func (Maybe (Type atom)) (Maybe (Type atom))
  | Tuple (Maybe (Type atom)) (Maybe (Type atom))
  deriving (Eq, Ord, Show)

data TypePair atom
  = TwoAtoms (Maybe atom) (Maybe atom)
  | TwoApps (Maybe (Type atom), Maybe (Type atom)) (Maybe (Type atom), Maybe (Type atom))
  | TwoFuncs (Maybe (Type atom), Maybe (Type atom)) (Maybe (Type atom), Maybe (Type atom))
  | TwoTuples (Maybe (Type atom), Maybe (Type atom)) (Maybe (Type atom), Maybe (Type atom))

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
  }

data TypeError err atom
  = AtomError err
  | StructureMismatch (Type atom) (Type atom)
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

structureUnify :: Maybe (Type atom) -> Maybe (Type atom) -> Either (TypeError err atom) (Maybe (TypePair atom))

structureUnify Nothing Nothing = Right Nothing

structureUnify (Just (Atom atom1)) (Just (Atom atom2)) = Right (Just (TwoAtoms (Just atom1) (Just atom2)))
structureUnify (Just (Atom atom1)) Nothing = Right (Just (TwoAtoms (Just atom1) Nothing))
structureUnify Nothing (Just (Atom atom2)) = Right (Just (TwoAtoms Nothing (Just atom2)))

structureUnify (Just (App head1 param1)) (Just (App head2 param2)) = Right (Just (TwoApps (head1, param1) (head2, param2)))
structureUnify (Just (App head1 param1)) Nothing = Right (Just (TwoApps (head1, param1) (Nothing, Nothing)))
structureUnify Nothing (Just (App head2 param2)) = Right (Just (TwoApps (Nothing, Nothing) (head2, param2)))

structureUnify (Just (Func arg1 ret1)) (Just (Func arg2 ret2)) = Right (Just (TwoFuncs (arg1, ret1) (arg2, ret2)))
structureUnify (Just (Func arg1 ret1)) Nothing = Right (Just (TwoFuncs (arg1, ret1) (Nothing, Nothing)))
structureUnify Nothing (Just (Func arg2 ret2)) = Right (Just (TwoFuncs (Nothing, Nothing) (arg2, ret2)))

structureUnify (Just (Tuple fst1 snd1)) (Just (Tuple fst2 snd2)) = Right (Just (TwoTuples (fst1, snd1) (fst2, snd2)))
structureUnify (Just (Tuple fst1 snd1)) Nothing = Right (Just (TwoTuples (fst1, snd1) (Nothing, Nothing)))
structureUnify Nothing (Just (Tuple fst2 snd2)) = Right (Just (TwoTuples (Nothing, Nothing) (fst2, snd2)))

structureUnify (Just bound1) (Just bound2) = Left (StructureMismatch bound1 bound2)

liftAtomUnifier :: forall err atom. Unifier err atom -> Unifier (TypeError err atom) (Type atom)
liftAtomUnifier atomUnifier =
  let
    typeUnifyEQ :: EQUnifier (TypeError err atom) (Type atom)
    typeUnifyEQ (Just bound1) (Just bound2) =
      case (bound1, bound2) of
        (Atom atom1, Atom atom2) ->
          mapEither AtomError (fmap Atom) (unifyEQ atomUnifier (Just atom1) (Just atom2))

        (App head1 param1, App head2 param2) -> do
          headBound <- typeUnifyEQ head1 head2
          paramBound <- typeUnifyEQ param1 param2
          return (Just (App headBound paramBound))

        (Func arg1 ret1, Func arg2 ret2) -> do
          arg <- typeUnifyEQ arg1 arg2
          ret <- typeUnifyEQ ret1 ret2
          return (Just (Func arg ret))

        (Tuple fst1 snd1, Tuple fst2 snd2) -> do
          fstBound <- typeUnifyEQ fst1 fst2
          sndBound <- typeUnifyEQ snd1 snd2
          return (Just (Tuple fstBound sndBound))

        (_, _) -> Left (StructureMismatch bound1 bound2)

    typeUnifyEQ bound1 bound2 = Right (bound1 <|> bound2)

    typeUnifyLTE :: LTEUnifier (TypeError err atom) (Type atom)
    typeUnifyLTE bound1 bound2 = do
      bounds <- structureUnify bound1 bound2
      case bounds of
        Just (TwoAtoms atom1 atom2) ->
          mapEither AtomError (mapBothSame (fmap Atom)) (unifyLTE atomUnifier atom1 atom2)

        Just (TwoApps (head1, param1) (head2, param2)) -> do
          (head1', head2') <- typeUnifyLTE head1 head2
          (param1', param2') <- fmap (dup (,)) (typeUnifyEQ param1 param2) -- type constructors are invariant in their type parameter
          return (Just (App head1' param1'), Just (App head2' param2'))

        Just (TwoFuncs (arg1, ret1) (arg2, ret2)) -> do
          (arg1', arg2') <- fmap swap (typeUnifyLTE arg2 arg1) -- functions are contravariant in their argument
          (ret1', ret2') <- typeUnifyLTE ret1 ret2
          return (Just (Func arg1' ret1'), Just (Func arg2' ret2'))

        Just (TwoTuples (fst1, snd1) (fst2, snd2)) -> do
          (fst1', fst2') <- typeUnifyLTE fst1 fst2
          (snd1', snd2') <- typeUnifyLTE snd1 snd2
          return (Just (Tuple fst1' snd1'), Just (Tuple fst2' snd2'))

        Nothing -> return (Nothing, Nothing)

    typeUnifyAsym :: AsymUnifier (TypeError err atom) (Type atom)
    typeUnifyAsym inequality bound1 bound2 = do
      bounds <- structureUnify bound1 bound2
      -- structureUnify will never return "Right Nothing" if one of its arguments is a "Just"
      case bounds of
        Just (TwoAtoms atom1 atom2) ->
          mapEither AtomError (fmap Atom) (unifyAsym atomUnifier inequality atom1 atom2)

        Just (TwoApps (head1, param1) (head2, _)) -> do
          head2' <- typeUnifyAsym inequality head1 head2
          return (Just (App head2' param1))

        Just (TwoFuncs (arg1, ret1) (arg2, ret2)) -> do
          arg2' <- typeUnifyAsym (flipInequality inequality) arg1 arg2
          ret2' <- typeUnifyAsym inequality ret1 ret2
          return (Just (Func arg2' ret2'))

        Just (TwoTuples (fst1, snd1) (fst2, snd2)) -> do
          fst2' <- typeUnifyAsym inequality fst1 fst2
          snd2' <- typeUnifyAsym inequality snd1 snd2
          return (Just (Tuple fst2' snd2'))

        Nothing -> Right Nothing
  in
    Unifier
      { unifyEQ = typeUnifyEQ
      , unifyLTE = typeUnifyLTE
      , unifyAsym = typeUnifyAsym
      }
