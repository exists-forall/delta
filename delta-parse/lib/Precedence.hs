module Precedence
  ( UngroupedTerm (..)
  , GroupedTerm (..)
  , Grouping (..)
  , group
  , substitute
  )
where

import qualified Data.Map as Map

{- TODO:
The algorithm implemented here is quadratic in the number of terms for left-associative operators.
It would be nice to improve this in the future.
-}

data UngroupedTerm a op = UngroupedTerm a (Maybe (op, UngroupedTerm a op))

data GroupedTerm a op
  = GroupedSingle a
  | GroupedBinary (GroupedTerm a op) op (GroupedTerm a op)
  deriving (Eq, Ord, Show)

data Grouping
  = GroupLeft
  | GroupRight
  deriving (Eq, Ord, Show)

group :: (op -> op -> Maybe Grouping) -> UngroupedTerm a op -> Either (op, op) (GroupedTerm a op)
group prec (UngroupedTerm x Nothing) = Right $ GroupedSingle x
group prec (UngroupedTerm x (Just (op, xs))) =
  insert =<< group prec xs where
    insert (GroupedSingle y) = Right $ GroupedBinary (GroupedSingle x) op (GroupedSingle y)
    insert (GroupedBinary y op' z) =
      case prec op op' of
        Just GroupRight -> Right $ GroupedBinary (GroupedSingle x) op (GroupedBinary y op' z)
        Just GroupLeft -> GroupedBinary <$> (insert y) <*> pure op' <*> pure z
        Nothing -> Left (op, op')

substitute :: (op -> a -> a -> a) -> GroupedTerm a op -> a
substitute _ (GroupedSingle x) = x
substitute f (GroupedBinary x op y) = f op (substitute f x) (substitute f y)
