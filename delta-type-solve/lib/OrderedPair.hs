-- By putting this simple logic in its own module, we can enforce
-- the invariant that all ordered pairs are actually ordered.

module OrderedPair
  ( OrderedPair -- note that constructor is *not* exported
  , orderedPair
  , orderedPair'
  , Flipped (..)
  , items
  )
where

data OrderedPair a = OrderedPair a a deriving (Eq, Ord, Show)

orderedPair :: (Ord a) => a -> a -> OrderedPair a
orderedPair x y =
  if x < y
    then OrderedPair x y
    else OrderedPair y x

data Flipped = DidFlip | DidNotFlip

orderedPair' :: (Ord a) => a -> a -> (OrderedPair a, Flipped)
orderedPair' x y =
  if x < y
    then (OrderedPair x y, DidNotFlip)
    else (OrderedPair y x, DidFlip)

items :: OrderedPair a -> (a, a)
items (OrderedPair x y) = (x, y)
