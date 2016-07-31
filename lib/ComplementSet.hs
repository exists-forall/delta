module ComplementSet
  ( ComplementSet (..)
  , complement
  , union
  , intersection
  , member
  )
where

import qualified Data.Set as Set
import Data.Set (Set)

data ComplementSet a
  = Excluded (Set a)
  | Included (Set a)
  deriving (Eq, Ord, Show)

complement :: ComplementSet a -> ComplementSet a
complement (Included set) = Excluded set
complement (Excluded set) = Included set

intersection :: (Ord a) => ComplementSet a -> ComplementSet a -> ComplementSet a
intersection (Included in1) (Included in2) = Included $ Set.intersection in1 in2
intersection (Excluded ex1) (Excluded ex2) = Excluded $ Set.union ex1 ex2
intersection (Included in1) (Excluded ex2) = Included $ in1 Set.\\ ex2
intersection (Excluded ex1) (Included in2) = Included $ in2 Set.\\ ex1

union :: (Ord a) => ComplementSet a -> ComplementSet a -> ComplementSet a
union set1 set2 = complement (intersection (complement set1) (complement set2))

member :: (Ord a) => a -> ComplementSet a -> Bool
member x (Included set) = Set.member x set
member x (Excluded set) = not (Set.member x set)
