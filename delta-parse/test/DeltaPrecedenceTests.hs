module DeltaPrecedenceTests (test) where

import Prelude hiding (div, and, or)

import Precedence
import DeltaPrecedence

import Test.Hspec

import Data.Either (isLeft)

import Delta.Structures.Syntax

-- Convenient aliases

uBin :: a -> op -> UngroupedTerm a op -> UngroupedTerm a op
uBin x op xs = UngroupedTerm x (Just (op, xs))

uSing :: a -> UngroupedTerm a op
uSing x = UngroupedTerm x Nothing

gBin :: GroupedTerm a op -> op -> GroupedTerm a op -> GroupedTerm a op
gBin = GroupedBinary

gSing :: a -> GroupedTerm a op
gSing = GroupedSingle

test :: Spec
test = describe "DeltaPrecedence" $ do
  let deltaGroup = group deltaGrouping

  let add = FunOp OpAdd
  let sub = FunOp OpSub
  let mul = FunOp OpMul
  let div = FunOp OpDiv

  let equ = FunOp OpEqu
  let lte = FunOp OpGTE
  let gte = FunOp OpGTE
  let and = FunOp OpAnd
  let or = FunOp OpOr

  let at = FunOp OpAt
  let compl = FunOp OpCompLeft
  let compr = FunOp OpCompRight

  let tup = TupleOp

  it "leaves single-atom expressions unchanged: a = a" $
    deltaGroup (uSing 'a') `shouldBe`
      Right (gSing 'a')

  it "leaves single-binary-operator expressions unchanged: a + b = (a + b)" $
    deltaGroup (uBin 'a' add $ uSing 'b') `shouldBe`
      Right
        (gBin
          (gSing 'a')
          add
          (gSing 'b'))

  it "correctly groups homogenous left-associative opertors: a + b + c = ((a + b) + c)" $
    deltaGroup (uBin 'a' add $ uBin 'b' add $ uSing 'c') `shouldBe`
      Right
        (gBin
          (gBin
            (gSing 'a')
            add
            (gSing 'b'))
          add
          (gSing 'c'))

  it "correctly groups homogenous right-associative operators: a, b, c = (a, (b, c))" $
    deltaGroup (uBin 'a' tup $ uBin 'b' tup $ uSing 'c') `shouldBe`
      Right
        (gBin
          (gSing 'a')
          tup
          (gBin
            (gSing 'b')
            tup
            (gSing 'c')))

  it "correctly groups mixed arithmetic operators: a - b / c * d + e = ((a - ((b / c) * d)) + e)" $
   deltaGroup (uBin 'a' sub $ uBin 'b' div $ uBin 'c' mul $ uBin 'd' add $ uSing 'e')
   `shouldBe`
      Right
        (gBin
          (gBin
            (gSing 'a')
            sub
            (gBin
              (gBin
                (gSing 'b')
                div
                (gSing 'c'))
              mul
              (gSing 'd')))
          add
          (gSing 'e'))

  it "correctly groups other operators: a == b || c >= d || e = ((a == b) || ((c >= d) || e))" $
    deltaGroup (uBin 'a' equ $ uBin 'b' or $ uBin 'c' gte $ uBin 'd' or $ uSing 'e') `shouldBe`
      Right
        (gBin
          (gBin
            (gSing 'a')
            equ
            (gSing 'b'))
          or
          (gBin
            (gBin
              (gSing 'c')
              gte
              (gSing 'd'))
            or
            (gSing 'e')))

  it "rejects homogenous mixed comparison operators: a == b == c" $
    deltaGroup (uBin 'a' equ $ uBin 'b' equ $ uSing 'c') `shouldSatisfy` isLeft

  it "rejects heterogeneous mixed comparison operators: a >= b <= c" $
    deltaGroup (uBin 'a' gte $ uBin 'b' lte $ uSing 'c') `shouldSatisfy` isLeft

  it "rejects heterogeneous mixed logical operators: a && b || c" $
    deltaGroup (uBin 'a' and $ uBin 'b' or $ uSing 'c') `shouldSatisfy` isLeft

  it "groups partial application as left-associative: f @ x @ y = ((f @ x) @ y)" $
    deltaGroup (uBin 'f' at $ uBin 'x' at $ uSing 'y') `shouldBe`
      Right
        (gBin
          (gBin
            (gSing 'f')
            at
            (gSing 'x'))
          at
          (gSing 'y'))

  it "groups left composition as left-associativite: f << g << h = ((f << g) << h)" $
    deltaGroup (uBin 'f' compl $ uBin 'g' compl $ uSing 'h') `shouldBe`
      Right
        (gBin
          (gBin
            (gSing 'f')
            compl
            (gSing 'g'))
          compl
          (gSing 'h'))

  it "groups right composition as right-associative: f >> g >> h = (f >> (g >> h))" $
    deltaGroup (uBin 'f' compr $ uBin 'g' compr $ uSing 'h') `shouldBe`
      Right
        (gBin
          (gSing 'f')
          compr
          (gBin
            (gSing 'g')
            compr
            (gSing 'h')))

  it "rejects mixed composition operators: f >> g << h" $
    deltaGroup (uBin 'f' compr $ uBin 'g' compl $ uSing 'h') `shouldSatisfy` isLeft
