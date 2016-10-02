module SyntaxUtils where

import Syntax

simpleIdent :: Letter -> Ident
simpleIdent l = Ident (Alpha LowerCase l) []

simpleUpperIdent :: Letter -> Ident
simpleUpperIdent l = Ident (Alpha UpperCase l) []

simpleVar :: Letter -> VarIdent
simpleVar l = VarIdent (simpleIdent l) $ BodySlot EmptyTail

simplePatVar :: Letter -> Pat
simplePatVar l = PatVar (simpleVar l) Nothing

simpleVarExpr :: Letter -> Expr
simpleVarExpr l = Var $ Path [] $ simpleVar l

simpleTIdent :: Letter -> TypeIdent
simpleTIdent l = TypeIdent l []

simpleTypeVar :: Letter -> TypeVarIdent
simpleTypeVar l = TypeVarIdent l []

simpleType :: Letter -> Type
simpleType l = TypeAtom $ Path [] $ simpleTIdent l

simpleModule :: Letter -> ModuleIdent
simpleModule l = ModuleIdent l []
