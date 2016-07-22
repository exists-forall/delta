{-# LANGUAGE ScopedTypeVariables, TupleSections #-}

module PosetSubsetSessionTests (test) where

import PosetSubsetSession

import Test.Hspec

import TestUtils (allSatisfy, allSame, allDifferent, iff, shouldBeUnordered)

import Data.Maybe (isJust, fromJust)
import Data.Tuple (swap)

import qualified Data.Set as Set

relations :: [Relation Char]
relations =
  [ 'A' `ChildOf` 'C'
  , 'A' `ChildOf` 'D'

  , 'B' `ChildOf` 'C'
  , 'B' `ChildOf` 'D'

  , 'C' `ChildOf` 'E'
  , 'C' `ChildOf` 'F'

  , 'D' `ChildOf` 'E'
  , 'D' `ChildOf` 'F'

  , 'G' `ChildOf` 'H'

  , 'H' `ChildOf` 'I'

  , 'J' `ChildOf` 'L'

  , 'K' `ChildOf` 'L'

  , 'L' `ChildOf` 'M'
  , 'L' `ChildOf` 'N'

  , 'O' `ChildOf` 'P'
  , 'O' `ChildOf` 'Q'

  , 'P' `ChildOf` 'R'

  , 'Q' `ChildOf` 'S'

  , 'R' `ChildOf` 'T'

  , 'S' `ChildOf` 'T'
  ]

testInSession :: forall s. (Char -> Node s Char) -> Spec
testInSession toNode = do
  let familialNodes = map toNode ['A'..'T']
  let [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t] = familialNodes
  let x = toNode 'X'
  let y = toNode 'Y'
  let top = topNode :: Node s Char
  let bot = botNode :: Node s Char

  describe "Node equality and 'toNode' function passed by runSession" $ do
    it "satisfies reflexivity for familial nodes" $
      familialNodes == familialNodes

    it "satisfies reflexivity for solitary nodes" $
      (x, y) == (x, y)

    it "considers distinct familial nodes unequal" $
      allDifferent familialNodes

    it "considers distinct solitary nodes unequal" $
      x /= y

    it "considers familial and solitary nodes unequal" $
      allDifferent (x : y : familialNodes)

    it "satisfies reflexivity for top and bottom nodes" $
      (top, bot) == (top, bot)

    it "considers top and bottom nodes unequal" $
      (top, bot) /= (bot, top)

    it "considers top and bottom nodes unequal to all other nodes" $
      allDifferent (top : bot : x : y : familialNodes)

  describe "'fromNode' function passed by runSession" $ do
    it "recovers the original value of familial nodes" $
      map fromNode familialNodes `shouldBe` map NodeValue ['A'..'T']

    it "recovers the original value of solitary nodes" $
      (fromNode x, fromNode y) `shouldBe` (NodeValue 'X', NodeValue 'Y')

    it "produces TopNodeValue when called with topNode" $
      (fromNode topNode :: NodeValue Char) `shouldBe` TopNodeValue

    it "produces BotNodeValue when called with botNode" $
      (fromNode botNode :: NodeValue Char) `shouldBe` BotNodeValue

  let
    expectedChildOfRelations =
      [ -- non-transitive / direct
        (a, c)
      , (d, f)

      , (o, q)
      , (s, t)

        -- transitive
      , (a, e)
      , (o, t)
      , (j, n)
      , (g, i)
      ]

  let expectedParentOfRelations = map swap expectedChildOfRelations

  let
    unexpectedChildOfRelations =
      expectedParentOfRelations ++
      [ (o, f)
      , (j, h)
      , (k, h)
      , (i, j)
      , (p, s)
      , (q, r)
      , (x, y)
      , (x, a)
      , (y, a)
      , (x, t)
      ]

  let unexpectedParentOfRelations = map swap unexpectedChildOfRelations

  let isChildOf (child, parent) = member child (extendWithChildren parent)
  let isParentOf (parent, child) = member parent (extendWithParents child)

  let
    containsAllAndOnly items subset =
      (top : bot : x : y : familialNodes) `allSatisfy` ((`member` subset) `iff` (`elem` items))

  describe "lower subsets" $ do
    describe "member" $ do
      it "considers all nodes children of topNode" $
        (top : bot : x : y : familialNodes) `allSatisfy` (isChildOf . (,top))

      it "considers no nodes except botNode children of botNode" $
        (top : x : y : familialNodes) `allSatisfy` (not . isChildOf . (,bot))

      it "considers botNode a child of botNode" $ do
        isChildOf (bot, bot)

      it "recognizes nontrivial true child-of relationships" $ do
        expectedChildOfRelations `allSatisfy` isChildOf

      it "recognizes nontrivial false child-of relationships" $ do
        unexpectedChildOfRelations `allSatisfy` (not . isChildOf)


    let universalSet = extendWithChildren topNode
    let trivialSet = extendWithChildren botNode
    let abcdeSet = intersection (extendWithChildren e) universalSet
    let oSet = intersection (extendWithChildren r) (extendWithChildren s)
    let abSet = intersection (extendWithChildren c) (extendWithChildren d)
    let jklSet = intersection (extendWithChildren m) (extendWithChildren n)
    let ghSet = intersection (extendWithChildren h) (extendWithChildren i)

    describe "intersection" $ do
      -- trivial cases

      it "treats the universal lower set as the identity element" $
        containsAllAndOnly [bot, a, b, c, d, e] abcdeSet

      it "treats the trivial lower set as the absorbing element" $
        containsAllAndOnly [bot] $ intersection (extendWithChildren e) trivialSet

      it "gives the trivial lower set as the intersection of disjoint familial subsets in the same family" $
        containsAllAndOnly [bot] $ intersection (extendWithChildren j) (extendWithChildren k)

      it "gives the trivial lower set as the intersection of disjoint familial subsets from different families" $
        containsAllAndOnly [bot] $ intersection (extendWithChildren h) (extendWithChildren l)

      it "gives the trivial lower set as the intersection of distinct solitary subsets" $
        containsAllAndOnly [bot] $ intersection (extendWithChildren x) (extendWithChildren y)

      it "is idempotent for familial subsets" $
        containsAllAndOnly [bot, a] $ intersection (extendWithChildren a) (extendWithChildren a)

      it "is idempotent for solitary subsets" $
        containsAllAndOnly [bot, x] $ intersection (extendWithChildren x) (extendWithChildren x)

      -- non-trivial cases

      it "finds unique shared children" $
        containsAllAndOnly [bot, o] oSet

      it "finds non-unique shared children" $
        containsAllAndOnly [bot, a, b] abSet

      it "finds transitive shared children" $
        containsAllAndOnly [bot, j, k, l] jklSet

      it "finds the smaller of two subsets when one is contained within the other" $
        containsAllAndOnly [bot, g, h] ghSet

    describe "maximal" $ do
      it "finds unique maximal nodes of singleton familial sets" $
        maximal oSet `shouldBeUnordered` [o]

      it "finds unique maximal nodes of non-singleton sets" $
        maximal jklSet `shouldBeUnordered` [l]

      it "returns the sole inhabitant of a solitary set" $
        maximal (extendWithChildren x) `shouldBeUnordered` [x]

      it "finds non-unique maximal nodes" $
        maximal abSet `shouldBeUnordered` [a, b]

      it "returns the bottom node for the trivial lower set" $
        maximal trivialSet `shouldBeUnordered` [bot]

      it "returns the top node for the universal lower set" $
        maximal universalSet `shouldBeUnordered` [top]

  describe "upper subsets" $ do
    describe "member" $ do
      it "considers all nodes parents of botNode" $
        (top : bot : x : y : familialNodes) `allSatisfy` (isParentOf . (,bot))

      it "considers no nodes except topNode parents of topNode" $
        (bot : x : y : familialNodes) `allSatisfy` (not . isParentOf . (,top))

      it "considers topNode a parent of topNode" $
        isParentOf (top, top)

      it "recognizes nontrivial true parent-of relationships" $
        expectedParentOfRelations `allSatisfy` isParentOf

      it "recognizes nontrivial false parent-of relationships" $
        unexpectedParentOfRelations `allSatisfy` (not . isParentOf)

    let universalSet = extendWithParents botNode
    let trivialSet = extendWithParents topNode
    let acdefSet = intersection (extendWithParents a) universalSet
    let tSet = intersection (extendWithParents p) (extendWithParents q)
    let cdefSet = intersection (extendWithParents a) (extendWithParents b)
    let lmnSet = intersection (extendWithParents j) (extendWithParents k)
    let hiSet = intersection (extendWithParents g) (extendWithParents h)

    describe "intersection" $ do
      -- trivial cases

      it "treats the universal upper set as the identity element" $
        containsAllAndOnly [top, a, c, d, e, f] acdefSet

      it "treats the trivial upper set as the absorbing element" $
        containsAllAndOnly [top] $ intersection (extendWithParents a) trivialSet

      it "gives the trivial upper set as the intersection of disjoint familial subsets in the same family" $
        containsAllAndOnly [top] $ intersection (extendWithParents m) (extendWithParents n)

      it "gives the trivial upper set as the intersection of disjoint familial subsets in different families" $
        containsAllAndOnly [top] $ intersection (extendWithParents o) (extendWithParents b)

      it "gives the trivial upper set as the intersection of distinct solitary subsets" $
        containsAllAndOnly [top] $ intersection (extendWithParents x) (extendWithParents y)

      it "is idempotent for familial subsets" $
        containsAllAndOnly [top, f] $ intersection (extendWithParents f) (extendWithParents f)

      it "is idempotent for solitary subsets" $
        containsAllAndOnly [top, x] $ intersection (extendWithParents x) (extendWithParents x)

      -- non-trivial cases

      it "finds unique shared parents" $
        containsAllAndOnly [top, t] tSet

      it "finds non-unique shared parents" $
        containsAllAndOnly [top, c, d, e, f] cdefSet

      it "finds transitive shared parents" $
        containsAllAndOnly [top, l, m, n] lmnSet

      it "finds the smaller of two subsets when one is contained within the other" $
        containsAllAndOnly [top, h, i] hiSet

    describe "minimal" $ do
      it "finds the unique minimal nodes of singleton familial sets" $
        minimal tSet `shouldBeUnordered` [t]

      it "finds unique minimal nodes of non-singleton sets" $
        minimal lmnSet `shouldBeUnordered` [l]

      it "returns the sole inhabitant of a solitary set" $
        minimal (extendWithParents x) `shouldBeUnordered` [x]

      it "finds non-unique minimal nodes" $
        minimal cdefSet `shouldBeUnordered` [c, d]

      it "returns the top node for the trivial upper set" $
        minimal trivialSet `shouldBeUnordered` [top]

      it "returns the bottom node for the universal upper set" $
        minimal universalSet `shouldBeUnordered` [bot]

test :: Spec
test = describe "PosetSubsetSession" $ do
  let poset = buildPoset relations
  describe "buildPoset" $ it "successfully builds well-formed posets" $ isJust poset
  runSession testInSession (fromJust poset)
