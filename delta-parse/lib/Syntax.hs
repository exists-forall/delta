module Syntax where

import Text.Parsec (SourcePos)

import Numeric.Natural (Natural)

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

data OperatorIdent
  -- Mathematical operators
  = OpAdd | OpSub | OpMul | OpDiv
  -- Comparison operators
  | OpEqu | OpNotEqu | OpGTE | OpLTE | OpLT | OpGT
  -- Logical operators
  | OpAnd | OpOr
  -- Functional operators
  | OpAt | OpCompLeft | OpCompRight
  deriving (Eq, Ord, Show)

data PrefixOperatorIdent
  = OpNegate
  | OpNot
  deriving (Eq, Ord, Show)

-- As with identifiers, this perfectly encodes the invariants of valid variable names.
data VarIdentTail = EmptyTail | TailWord Ident VarIdentTail | TailSlot VarIdentTail deriving (Eq, Ord, Show)
data VarIdentBody = BodyWord Ident VarIdentBody | BodySlot VarIdentTail deriving (Eq, Ord, Show)
data VarIdent
  = VarIdent Ident VarIdentBody
  | DotVarIdent Ident VarIdentTail
  | OperatorIdent OperatorIdent
  | PrefixOperatorIdent PrefixOperatorIdent
  deriving (Eq, Ord, Show)

-- All types begin with an uppercase letter
data TypeIdent = TypeIdent Letter [IdentChar] deriving (Eq, Ord, Show)

-- All type variables begin with a lowercase letter
data TypeVarIdent = TypeVarIdent Letter [IdentChar] deriving (Eq, Ord, Show)

-- All module names begin with an uppercase letter
data ModuleIdent = ModuleIdent Letter [IdentChar] deriving (Eq, Ord, Show)
data Path a = Path [ModuleIdent] a deriving (Eq, Ord, Show)

data StringComponent = Char Char | Interpolate Expr deriving (Eq, Ord, Show)

data Pat' annot
  = PatVar VarIdent annot
  | PatTuple (Pat' annot) (Pat' annot)
  | PatIgnore annot
  | PatUnit
  | MarkPat SourcePos (Pat' annot) SourcePos
  deriving (Eq, Ord, Show)

type Pat = Pat' (Maybe Type)
type TypedPat = Pat' Type

data Expr
  = Var (Path VarIdent)
  | LitUInt Natural
  | LitString [StringComponent]
  | LitSeq [Expr]
  | Unit
  | Tuple Expr Expr
  | Call Expr Expr
  | Func Pat Expr
  | Let Pat Expr Expr
  | Mark SourcePos Expr SourcePos
  deriving (Eq, Ord, Show)

data Type
  = TypeAtom (Path TypeIdent)
  | TypeVar TypeVarIdent
  | TypeApp Type Type
  | TypeTuple Type Type
  | TypeFunc Type Type Type
  | TypeInters Type Type
  | TypePure
  | TypeUnit
  | TypeNever
  | MarkType SourcePos Type SourcePos
  deriving (Eq, Ord, Show)

data StructComponent
  = StructField Ident Type
  | StructCase TypeIdent [StructComponent]
  deriving (Eq, Ord, Show)

data Constraint
  = ConstraintImplement (Path TypeIdent) (Type)
  deriving (Eq, Ord, Show)

data Decl
  = DeclDef TypedPat [Constraint] Expr
  | DeclTypeStruct TypeIdent [TypeVarIdent] [StructComponent]
  | DeclProtocol TypeIdent TypeVarIdent [Stub]
  | DeclImplement (Path TypeIdent) Type [Constraint] [(TypedPat, [Constraint], Expr)]
  | DeclInteraction TypeIdent [TypeVarIdent] [(Ident, Type, Type)]
  | MarkDecl SourcePos Decl SourcePos
  deriving (Eq, Ord, Show)

data Stub
  = StubDef VarIdent Type [Constraint]
  | StubImplement (Path TypeIdent) Type [Constraint]
  deriving (Eq, Ord, Show)

-- For testing purposes:

stripPatMarks :: (annot -> annot) -> Pat' annot -> Pat' annot
stripPatMarks stripAnnot (PatVar v t) = PatVar v (stripAnnot t)
stripPatMarks stripAnnot (PatTuple a b) =
  PatTuple (stripPatMarks stripAnnot a) (stripPatMarks stripAnnot b)
stripPatMarks stripAnnot (PatIgnore t) = PatIgnore (stripAnnot t)
stripPatMarks _ PatUnit = PatUnit
stripPatMarks stripAnnot (MarkPat _ pat _) = stripPatMarks stripAnnot pat

stripMarks :: Expr -> Expr
stripMarks (Var v) = Var v
stripMarks (LitUInt i) = LitUInt i
stripMarks (LitString s) = LitString (map stripComponentMarks s) where
  stripComponentMarks (Char c) = Char c
  stripComponentMarks (Interpolate e) = Interpolate (stripMarks e)
stripMarks (LitSeq xs) = LitSeq (map stripMarks xs)
stripMarks Unit = Unit
stripMarks (Tuple a b) = Tuple (stripMarks a) (stripMarks b)
stripMarks (Call a b) = Call (stripMarks a) (stripMarks b)
stripMarks (Func pat ret) = Func (stripPatMarks (fmap stripTypeMarks) pat) (stripMarks ret)
stripMarks (Let pat e ret) =
  Let (stripPatMarks (fmap stripTypeMarks) pat) (stripMarks e) (stripMarks ret)
stripMarks (Mark _ e _) = stripMarks e

stripTypeMarks :: Type -> Type
stripTypeMarks (TypeAtom a) = TypeAtom a
stripTypeMarks (TypeVar v) = TypeVar v
stripTypeMarks (TypeApp c p) = TypeApp (stripTypeMarks c) (stripTypeMarks p)
stripTypeMarks (TypeTuple l r) = TypeTuple (stripTypeMarks l) (stripTypeMarks r)
stripTypeMarks (TypeFunc a i r) = TypeFunc (stripTypeMarks a) (stripTypeMarks i) (stripTypeMarks r)
stripTypeMarks (TypeInters a b) = TypeInters (stripTypeMarks a) (stripTypeMarks b)
stripTypeMarks TypePure = TypePure
stripTypeMarks TypeNever = TypeNever
stripTypeMarks TypeUnit = TypeUnit
stripTypeMarks (MarkType _ t _) = stripTypeMarks t

stripStructComponentMarks :: StructComponent -> StructComponent
stripStructComponentMarks (StructField i t) = StructField i (stripTypeMarks t)
stripStructComponentMarks (StructCase i cs) = StructCase i (map stripStructComponentMarks cs)

stripConstraintMarks :: Constraint -> Constraint
stripConstraintMarks (ConstraintImplement p t) = ConstraintImplement p (stripTypeMarks t)

stripDeclMarks :: Decl -> Decl
stripDeclMarks (DeclDef p c e) =
  DeclDef (stripPatMarks stripTypeMarks p) (map stripConstraintMarks c) (stripMarks e)
stripDeclMarks (DeclTypeStruct t vs cs) = DeclTypeStruct t vs (map stripStructComponentMarks cs)
stripDeclMarks (DeclProtocol p t s) = DeclProtocol p t (map stripStubMarks s)
stripDeclMarks (DeclImplement p t c ds) =
  DeclImplement p (stripTypeMarks t) (map stripConstraintMarks c) $
    flip map ds $ \(pat, csts, expr) ->
      (stripPatMarks stripTypeMarks pat, (map stripConstraintMarks csts), stripMarks expr)
stripDeclMarks (DeclInteraction i v ms) =
  DeclInteraction i v $
    map (\(name, call, resp) -> (name, stripTypeMarks call, stripTypeMarks resp)) ms
stripDeclMarks (MarkDecl _ d _) = stripDeclMarks d

stripStubMarks :: Stub -> Stub
stripStubMarks (StubDef i t c) = StubDef i (stripTypeMarks t) (map stripConstraintMarks c)
stripStubMarks (StubImplement p t c) = StubImplement p (stripTypeMarks t) (map stripConstraintMarks c)
