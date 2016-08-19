module Syntax where

import Text.Parsec (SourcePos)

{-
This might seem like madness, but there is an exact one-to-one correspondence between valid Delta
identifiers and the inhabitants of this type.  This representation perfectly encodes the invariants.
-}
data Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Eq, Ord, Show, Enum)
data Capitalization = LowerCase | UpperCase deriving (Eq, Ord, Show)
data IdentStartChar = Alpha Capitalization Letter | Underscore deriving (Eq, Ord, Show)
data Digit = D0|D1|D2|D3|D4|D5|D6|D7|D8|D9 deriving (Eq, Ord, Show, Enum)
data IdentChar = StartChar IdentStartChar | Digit Digit deriving (Eq, Ord, Show)
data Ident = Ident IdentStartChar [IdentChar] deriving (Eq, Ord, Show)

-- As with identifiers, this perfectly encodes the invariants of valid variable names.
data VarIdentTail = EmptyTail | TailWord Ident VarIdentTail | TailSlot VarIdentTail deriving (Eq, Ord, Show)
data VarIdentBody = BodyWord Ident VarIdentBody | BodySlot VarIdentTail deriving (Eq, Ord, Show)
data VarIdent = VarIdent Ident VarIdentBody | DotVarIdent Ident VarIdentTail deriving (Eq, Ord, Show)

-- All types begin with an uppercase letter
data TypeIdent = TypeIdent Letter [IdentChar] deriving (Eq, Ord, Show)

-- All type variables begin with a lowercase letter
data TypeVarIdent = TypeVarIdent Letter [IdentChar] deriving (Eq, Ord, Show)

-- All module names begin with an uppercase letter
data ModuleIdent = ModuleIdent Letter [IdentChar] deriving (Eq, Ord, Show)
data Path a = Path [ModuleIdent] a deriving (Eq, Ord, Show)

type VarPath = Path VarIdent
type TypePath = Path TypeIdent

data Expr
  = Var (Path VarIdent)
  | Unit
  | Tuple Expr Expr
  | Call Expr Expr
  | Mark SourcePos Expr SourcePos
  deriving (Eq, Ord, Show)

-- For testing purposes:
stripMarks :: Expr -> Expr
stripMarks (Var v) = Var v
stripMarks Unit = Unit
stripMarks (Tuple a b) = Tuple (stripMarks a) (stripMarks b)
stripMarks (Call a b) = Call (stripMarks a) (stripMarks b)
stripMarks (Mark _ e _) = stripMarks e
