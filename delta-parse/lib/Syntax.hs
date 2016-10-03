{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Syntax where

import GHC.Generics (Generic)

import qualified Data.Aeson.Types as Aeson
import Data.Text.Lazy (fromStrict)
import Data.Text (unpack)

import Data.Bifunctor (second)

import ParseUtils (Parser, fullParse, SourcePos)
import {-# SOURCE #-} ParseIdent
import {-# SOURCE #-} FormatIdent

-- This is an internal utility function, so it really shouldn't be exported
toJSONParser :: String -> (CheckReserved -> Parser a) -> Aeson.Value -> Aeson.Parser a

toJSONParser typeName p (Aeson.String src) =
  case fullParse (p AllowReserved) (fromStrict src) of
    Right result -> return result
    Left _ -> fail ("Malformed " ++ typeName ++ ": " ++ unpack src)

toJSONParser typeName _ x =
  Aeson.typeMismatch typeName x

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

instance Aeson.FromJSON Ident where
  parseJSON = toJSONParser "Ident" ident'

instance Aeson.ToJSON Ident where
  toJSON = Aeson.String . formatIdent

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

instance Aeson.FromJSON VarIdent where
  parseJSON = toJSONParser "VarIdent" varIdent'

instance Aeson.ToJSON VarIdent where
  toJSON = Aeson.String . formatVarIdent

-- All types begin with an uppercase letter
data TypeIdent = TypeIdent Letter [IdentChar] deriving (Eq, Ord, Show)

instance Aeson.FromJSON TypeIdent where
  parseJSON = toJSONParser "TypeIdent" typeIdent'

instance Aeson.ToJSON TypeIdent where
  toJSON = Aeson.String . formatTypeIdent

-- All type variables begin with a lowercase letter
data TypeVarIdent = TypeVarIdent Letter [IdentChar] deriving (Eq, Ord, Show)

instance Aeson.FromJSON TypeVarIdent where
  parseJSON = toJSONParser "TypeVarIdent" typeVarIdent'

instance Aeson.ToJSON TypeVarIdent where
  toJSON = Aeson.String . formatTypeVarIdent

-- All module names begin with an uppercase letter
data ModuleIdent = ModuleIdent Letter [IdentChar] deriving (Eq, Ord, Show)

instance Aeson.FromJSON ModuleIdent where
  parseJSON = toJSONParser "ModuleIdent" moduleIdent'

instance Aeson.ToJSON ModuleIdent where
  toJSON = Aeson.String . formatModuleIdent

data Path a
  = Path
    { path :: [ModuleIdent]
    , name :: a
    }
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data StringComponent
  = Chars
    { chars :: String
    }
  | Interpolate
    { interpolate_expr :: Expr
    }
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data Pat' annot
  = PatVar
    { pat_var_name :: VarIdent
    , pat_var_type :: annot
    }
  | PatTuple
    { pat_tuple_left :: Pat' annot
    , pat_tuple_right :: Pat' annot
    }
  | PatIgnore
    { pat_ignore_type :: annot
    }
  | PatUnit
  | MarkPat
    { pat_source_start :: SourcePos
    , enclosed_pat :: Pat' annot
    , pat_source_end :: SourcePos
    }
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

type Pat = Pat' (Maybe Type)
type TypedPat = Pat' Type

data Expr
  = Var
    { var_name :: Path VarIdent
    }
  | LitUInt
    { uint_value :: Integer
    }
  | LitString
    { string_components :: [StringComponent]
    }
  | LitSeq
    { seq_items :: [Expr]
    }
  | Unit
  | Tuple
    { tuple_left :: Expr
    , tuple_right :: Expr
    }
  | Call
    { call_function :: Expr
    , call_argument :: Expr
    }
  | Func
    { func_params_pat :: Pat
    , func_body :: Expr
    }
  | Let
    { let_pat :: Pat
    , let_value :: Expr
    , let_body :: Expr
    }
  | PartialCallChain
    { partial_calls :: [(Path VarIdent, Maybe Expr)]
    }
  -- NOTE: PartialCallChain encodes neither the invariant that the VarIdent should always be a
  -- DotVarIdent, nor the inavriant that the list of composed functions should always be nonempty.
  -- What should be done about this?
  | Mark
    { source_start :: SourcePos
    , enclosed :: Expr
    , source_end :: SourcePos
    }
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data Type
  = TypeAtom
    { type_name :: Path TypeIdent
    }
  | TypeVar
    { type_var_name :: TypeVarIdent
    }
  | TypeApp
    { type_head :: Type
    , type_argument :: Type
    }
  | TypeTuple
    { type_tuple_left :: Type
    , type_tuple_right :: Type
    }
  | TypeFunc
    { type_func_argument :: Type
    , type_func_interaction :: Type
    , type_func_return :: Type
    }
  | TypeInters
    { type_inters_left :: Type
    , type_inters_right :: Type
    }
  | TypePure
  | TypeUnit
  | TypeNever
  | MarkType
    { type_source_start :: SourcePos
    , enclosed_type :: Type
    , type_source_end :: SourcePos
    }
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data StructComponent
  = StructField
    { struct_field_name :: Ident
    , struct_field_type :: Type
    }
  | StructCase
    { struct_case_name :: TypeIdent
    , struct_case_body :: [StructComponent]
    }
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data Constraint
  = ConstraintImplement
    { constraint_implement_protocol :: Path TypeIdent
    , constraint_implement_argument :: Type
    }
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data Decl
  = DeclDef
    { decl_def_pat :: TypedPat
    , decl_def_constraints :: [Constraint]
    , decl_def_value :: Expr
    }
  | DeclTypeStruct
    { decl_type_struct_name :: TypeIdent
    , decl_type_struct_params :: [TypeVarIdent]
    , decl_type_struct_body :: [StructComponent]
    }
  | DeclProtocol
    { decl_protocol_name :: TypeIdent
    , decl_protocol_param :: TypeVarIdent
    , decl_protocol_body :: [Stub]
    }
  | DeclImplement
    { decl_implement_protocol :: Path TypeIdent
    , decl_implement_argument :: Type
    , decl_implement_constraints :: [Constraint]
    , decl_implement_body :: [(TypedPat, [Constraint], Expr)]
    }
  | DeclInteraction
    { decl_interaction_name :: TypeIdent
    , decl_interaction_params :: [TypeVarIdent]
    , decl_interaction_body :: [(Ident, Type, Type)]
    }
  | MarkDecl
    { decl_source_start :: SourcePos
    , enclosed_decl :: Decl
    , decl_source_end :: SourcePos
    }
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data Stub
  = StubDef
    { stub_def_name :: VarIdent
    , stub_def_type :: Type
    , stub_def_constraints :: [Constraint]
    }
  | StubImplement
    { stub_implement_protocol :: Path TypeIdent
    , stub_implement_argument :: Type
    , stub_implement_constraints :: [Constraint]
    }
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data PossibleAlias a
  = Alias
    { symbol_new_name :: a
    , symbol_original_name :: a
    }
  | NoAlias
    { symbol_name :: a
    }
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data Symbol
  = SymbolType
    { symbol_type :: PossibleAlias TypeIdent
    }
  | SymbolInteraction
    { symbol_interaction :: PossibleAlias TypeIdent
    }
  | SymbolProtocol
    { symbol_protocol :: PossibleAlias TypeIdent
    }
  | SymbolDef
    { symbol_def :: PossibleAlias VarIdent
    }
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data Symbols
  = SymbolsSpecific
    { symbols_specific :: [Symbol]
    }
  | SymbolsEverything
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data Import
  = Import
    { import_module :: PossibleAlias (Path ModuleIdent)
    , import_symbols :: Symbols
    }
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data Module
  = Module
    { module_export_symbols :: Symbols
    , module_imports :: [Import]
    , module_body :: [Decl]
    }
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

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
  stripComponentMarks (Chars c) = Chars c
  stripComponentMarks (Interpolate e) = Interpolate (stripMarks e)
stripMarks (LitSeq xs) = LitSeq (map stripMarks xs)
stripMarks Unit = Unit
stripMarks (Tuple a b) = Tuple (stripMarks a) (stripMarks b)
stripMarks (Call a b) = Call (stripMarks a) (stripMarks b)
stripMarks (Func pat ret) = Func (stripPatMarks (fmap stripTypeMarks) pat) (stripMarks ret)
stripMarks (Let pat e ret) =
  Let (stripPatMarks (fmap stripTypeMarks) pat) (stripMarks e) (stripMarks ret)
stripMarks (PartialCallChain cs) = PartialCallChain (map (second (fmap stripMarks)) cs)
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

stripModuleMarks :: Module -> Module
stripModuleMarks (Module x i d) = Module x i (map stripDeclMarks d)
