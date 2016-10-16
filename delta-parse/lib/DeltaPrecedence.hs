module DeltaPrecedence
  ( BinaryOperator (..)
  , deltaGrouping
  )
where

import Precedence

import qualified Delta.Structures.Syntax as Stx

data BinaryOperator
  = FunOp Stx.OperatorIdent
  | TupleOp
  deriving (Eq, Ord, Show)

data ArithmeticPrecedence
  = MulPrecedence
  | AddPrecedence
  deriving (Eq, Ord)

data Precedence
  = ArithmeticPrecedence ArithmeticPrecedence
  | ComparisonPrecedence
  | LogicalPrecedence
  | PartialApplicationPrecedence
  | CompositionPrecedence
  | TuplePrecedence
  deriving (Eq, Ord)

precedence :: BinaryOperator -> Precedence

precedence (FunOp Stx.OpAdd) = ArithmeticPrecedence AddPrecedence
precedence (FunOp Stx.OpSub) = ArithmeticPrecedence AddPrecedence

precedence (FunOp Stx.OpMul) = ArithmeticPrecedence MulPrecedence
precedence (FunOp Stx.OpDiv) = ArithmeticPrecedence MulPrecedence

precedence (FunOp Stx.OpEqu) = ComparisonPrecedence
precedence (FunOp Stx.OpNotEqu) = ComparisonPrecedence
precedence (FunOp Stx.OpGTE) = ComparisonPrecedence
precedence (FunOp Stx.OpLTE) = ComparisonPrecedence
precedence (FunOp Stx.OpLT) = ComparisonPrecedence
precedence (FunOp Stx.OpGT) = ComparisonPrecedence

precedence (FunOp Stx.OpAnd) = LogicalPrecedence
precedence (FunOp Stx.OpOr) = LogicalPrecedence

precedence (FunOp Stx.OpAt) = PartialApplicationPrecedence

precedence (FunOp Stx.OpCompLeft) = CompositionPrecedence
precedence (FunOp Stx.OpCompRight) = CompositionPrecedence

precedence TupleOp = TuplePrecedence

deltaGrouping :: BinaryOperator -> BinaryOperator -> Maybe Grouping
deltaGrouping op1 op2 =
  let
    prec1 = precedence op1
    prec2 = precedence op2
  in case prec1 `compare` prec2 of
    LT -> Just GroupLeft
    GT -> Just GroupRight
    EQ ->
      case prec1 of
        ArithmeticPrecedence _ -> Just GroupLeft
        ComparisonPrecedence -> Nothing
        LogicalPrecedence ->
          if op1 == op2
              then Just GroupRight
              else Nothing
        PartialApplicationPrecedence -> Just GroupLeft
        CompositionPrecedence ->
          case (op1, op2) of
            (FunOp Stx.OpCompLeft, FunOp Stx.OpCompLeft) -> Just GroupLeft
            (FunOp Stx.OpCompRight, FunOp Stx.OpCompRight) -> Just GroupRight
            (_, _) -> Nothing
        TuplePrecedence -> Just GroupRight
