{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Delta.Structures.Syntax
  ( SourcePos (..)

  , Letter (..)
  , Capitalization (..)
  , IdentStartChar (..)
  , Digit (..)
  , IdentChar (..)

  , Ident (..)
  , IdentText
  , identText
  , unwrapIdentText

  , OperatorIdent (..)
  , PrefixOperatorIdent (..)
  , VarIdentTail (..)
  , VarIdentBody (..)

  , VarIdent (..)
  , VarIdentText
  , varIdentText
  , unwrapVarIdentText

  , TypeIdent (..)
  , TypeIdentText
  , typeIdentText
  , unwrapTypeIdentText

  , TypeVarIdent (..)
  , TypeVarIdentText
  , typeVarIdentText
  , unwrapTypeVarIdentText

  , ModuleIdent (..)
  , ModuleIdentText
  , moduleIdentText
  , unwrapModuleIdentText

  , Path (..)

  , StringComponent (..)
  , Pat' (..)
  , Pat
  , TypedPat

  , Expr (..)

  , Type (..)

  , StructComponent (..)
  , Constraint (..)
  , Decl (..)
  , Stub (..)

  , PossibleAlias (..)
  , Symbol (..)
  , Symbols (..)
  , Import (..)
  , Module (..)
  )
where

import GHC.Generics (Generic)

import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)

import {-# SOURCE #-} Utils.FormatIdent

data SourcePos
  = SourcePos
    { line :: Int
    , col :: Int
    }
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

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

newtype IdentText
  = IdentText Text
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

identText :: Ident -> IdentText
identText = IdentText . formatIdent

unwrapIdentText :: IdentText -> Text
unwrapIdentText (IdentText t) = t

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
data VarIdentTail = EmptyTail | TailWord IdentText VarIdentTail | TailSlot VarIdentTail deriving (Eq, Ord, Show)
data VarIdentBody = BodyWord IdentText VarIdentBody | BodySlot VarIdentTail deriving (Eq, Ord, Show)
data VarIdent
  = VarIdent IdentText VarIdentBody
  | DotVarIdent IdentText VarIdentTail
  | OperatorIdent OperatorIdent
  | PrefixOperatorIdent PrefixOperatorIdent
  deriving (Eq, Ord, Show)

newtype VarIdentText
  = VarIdentText Text
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

varIdentText :: VarIdent -> VarIdentText
varIdentText = VarIdentText . formatVarIdent

unwrapVarIdentText :: VarIdentText -> Text
unwrapVarIdentText (VarIdentText t) = t

-- All types begin with an uppercase letter
data TypeIdent = TypeIdent Letter [IdentChar] deriving (Eq, Ord, Show)

newtype TypeIdentText
  = TypeIdentText Text
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

typeIdentText :: TypeIdent -> TypeIdentText
typeIdentText = TypeIdentText . formatTypeIdent

unwrapTypeIdentText :: TypeIdentText -> Text
unwrapTypeIdentText (TypeIdentText t) = t

-- All type variables begin with a lowercase letter
data TypeVarIdent = TypeVarIdent Letter [IdentChar] deriving (Eq, Ord, Show)

newtype TypeVarIdentText
  = TypeVarIdentText Text
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

typeVarIdentText :: TypeVarIdent -> TypeVarIdentText
typeVarIdentText = TypeVarIdentText . formatTypeVarIdent

unwrapTypeVarIdentText :: TypeVarIdentText -> Text
unwrapTypeVarIdentText (TypeVarIdentText t) = t

-- All module names begin with an uppercase letter
data ModuleIdent = ModuleIdent Letter [IdentChar] deriving (Eq, Ord, Show)

newtype ModuleIdentText
  = ModuleIdentText Text
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

moduleIdentText :: ModuleIdent -> ModuleIdentText
moduleIdentText = ModuleIdentText . formatModuleIdent

unwrapModuleIdentText :: ModuleIdentText -> Text
unwrapModuleIdentText (ModuleIdentText t) = t

data Path a
  = Path
    { path :: [ModuleIdentText]
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
    { pat_var_name :: VarIdentText
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
    { var_name :: Path VarIdentText
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
    { partial_calls :: [(Path VarIdentText, Maybe Expr)]
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
    { type_name :: Path TypeIdentText
    }
  | TypeVar
    { type_var_name :: TypeVarIdentText
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
    { struct_field_name :: IdentText
    , struct_field_type :: Type
    }
  | StructCase
    { struct_case_name :: TypeIdentText
    , struct_case_body :: [StructComponent]
    }
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data Constraint
  = ConstraintImplement
    { constraint_implement_protocol :: Path TypeIdentText
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
    { decl_type_struct_name :: TypeIdentText
    , decl_type_struct_params :: [TypeVarIdentText]
    , decl_type_struct_body :: [StructComponent]
    }
  | DeclProtocol
    { decl_protocol_name :: TypeIdentText
    , decl_protocol_param :: TypeVarIdentText
    , decl_protocol_body :: [Stub]
    }
  | DeclImplement
    { decl_implement_protocol :: Path TypeIdentText
    , decl_implement_argument :: Type
    , decl_implement_constraints :: [Constraint]
    , decl_implement_body :: [(TypedPat, [Constraint], Expr)]
    }
  | DeclInteraction
    { decl_interaction_name :: TypeIdentText
    , decl_interaction_params :: [TypeVarIdentText]
    , decl_interaction_body :: [(IdentText, Type, Type)]
    }
  | MarkDecl
    { decl_source_start :: SourcePos
    , enclosed_decl :: Decl
    , decl_source_end :: SourcePos
    }
  deriving (Eq, Ord, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data Stub
  = StubDef
    { stub_def_name :: VarIdentText
    , stub_def_type :: Type
    , stub_def_constraints :: [Constraint]
    }
  | StubImplement
    { stub_implement_protocol :: Path TypeIdentText
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
    { symbol_type :: PossibleAlias TypeIdentText
    }
  | SymbolInteraction
    { symbol_interaction :: PossibleAlias TypeIdentText
    }
  | SymbolProtocol
    { symbol_protocol :: PossibleAlias TypeIdentText
    }
  | SymbolDef
    { symbol_def :: PossibleAlias VarIdentText
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
    { import_module :: PossibleAlias (Path ModuleIdentText)
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
