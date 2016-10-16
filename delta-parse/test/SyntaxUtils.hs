module SyntaxUtils where

import Syntax

simpleIdent :: Letter -> IdentText
simpleIdent l = identText $ Ident (Alpha LowerCase l) []

simpleUpperIdent :: Letter -> IdentText
simpleUpperIdent l = identText $ Ident (Alpha UpperCase l) []

simpleVar :: Letter -> VarIdentText
simpleVar l = varIdentText $ VarIdent (simpleIdent l) $ BodySlot EmptyTail

simplePatVar :: Letter -> Pat
simplePatVar l = PatVar (simpleVar l) Nothing

simpleVarExpr :: Letter -> Expr
simpleVarExpr l = Var $ Path [] $ simpleVar l

simpleTIdent :: Letter -> TypeIdentText
simpleTIdent l = typeIdentText $ TypeIdent l []

simpleTypeVar :: Letter -> TypeVarIdentText
simpleTypeVar l = typeVarIdentText $ TypeVarIdent l []

simpleType :: Letter -> Type
simpleType l = TypeAtom $ Path [] $ simpleTIdent l

simpleModule :: Letter -> ModuleIdentText
simpleModule l = moduleIdentText $ ModuleIdent l []

typeIdentPure :: TypeIdentText
typeIdentPure = typeIdentText $ TypeIdent P $ map (StartChar . Alpha LowerCase) [U, R, E]
