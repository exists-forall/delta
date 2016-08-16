module IndexSet
  ( IndexSet

  , empty
  , singleton
  , fromIndices

  , member
  , indices

  , union
  , intersection
  , (\\)
  )
where

import Data.Int (Int32)
import Data.Bits ((.&.), (.|.), finiteBitSize, setBit, testBit, complement)

type Segment = Int32

newtype IndexSet = IndexSet [Segment] deriving (Eq, Ord)
-- Equality on IndexSets is reliable, because of the invariant, preserved by all manipulation
-- functions, that a IndexSet never has a tail of segments which consist entirely of zeroes.
-- In other words, the last segment of a IndexSet, if there is one, is always nonzero.

-- Ordering on IndexSets is reliable and well-behaved, but does NOT represent set inclusion.

instance Show IndexSet where
  showsPrec prec set =
    showParen (prec > 10) (showString "fromIndices " . shows (indices set))

normalize :: [Segment] -> [Segment]
normalize [] = []
normalize (0 : xs) =
  case normalize xs of
    [] -> []
    xs' -> 0 : xs'
normalize (x : xs) = x : normalize xs

-- Smart constructor for preserving invariants
indexSet :: [Segment] -> IndexSet
indexSet = IndexSet . normalize

empty :: IndexSet
empty = IndexSet []

bitsPerSeg :: Int
bitsPerSeg = finiteBitSize (0 :: Segment)

singleton' :: Int -> [Segment]
singleton' i =
  if i < bitsPerSeg
    then [setBit 0 i]
    else (0 : singleton' (i - bitsPerSeg))

singleton :: Int -> IndexSet
singleton = IndexSet . singleton'

fromIndices :: [Int] -> IndexSet
fromIndices = foldl union empty . map singleton

member' :: Int -> [Segment] -> Bool
member' _ [] = False
member' i (x : xs) =
  if i < bitsPerSeg
    then testBit x i
    else member' (i - bitsPerSeg) xs

member :: Int -> IndexSet -> Bool
member i (IndexSet segs) = member' i segs

union' :: [Segment] -> [Segment] -> [Segment]
union' xs [] = xs
union' [] ys = ys
union' (x : xs) (y : ys) = (x .|. y) : (union' xs ys)

union :: IndexSet -> IndexSet -> IndexSet
union (IndexSet xs) (IndexSet ys) = indexSet (union' xs ys)

intersection :: IndexSet -> IndexSet -> IndexSet
intersection (IndexSet xs) (IndexSet ys) = indexSet (zipWith (.&.) xs ys)

diff' :: [Segment] -> [Segment] -> [Segment]
diff' xs [] = xs
diff' [] _  = []
diff' (x : xs) (y : ys) =
  (x .&. complement y) : (diff' xs ys)

(\\) :: IndexSet -> IndexSet -> IndexSet
(IndexSet xs) \\ (IndexSet ys) = indexSet (diff' xs ys)

indices' :: [Segment] -> [Int]
indices' xs =
  let
    withOffsets = zip [0, bitsPerSeg ..] xs
    indicesOfSegment (offset, x) = map (+ offset) $ filter (testBit x) [0..bitsPerSeg - 1]
  in
    concatMap indicesOfSegment withOffsets

indices :: IndexSet -> [Int]
indices (IndexSet xs) = indices' xs
