module StripRedundantMarks where

import Syntax
import Data.Bifunctor (second)

stripPatMarks :: (annot -> annot) -> Pat' annot -> Pat' annot
stripPatMarks stripAnnot (PatVar v t) = PatVar v (stripAnnot t)
stripPatMarks stripAnnot (PatTuple a b) =
  PatTuple (stripPatMarks stripAnnot a) (stripPatMarks stripAnnot b)
stripPatMarks stripAnnot (PatIgnore t) = PatIgnore (stripAnnot t)
stripPatMarks _ PatUnit = PatUnit
stripPatMarks stripAnnot (MarkPat _ pat@(MarkPat _ _ _) _) = stripPatMarks stripAnnot pat
stripPatMarks stripAnnot (MarkPat start p end) = MarkPat start (stripPatMarks stripAnnot p) end

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
stripMarks (Mark _ e@(Mark _ _ _) _) = stripMarks e
stripMarks (Mark start e end) = Mark start (stripMarks e) end

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
stripTypeMarks (MarkType _ t@(MarkType _ _ _) _) = stripTypeMarks t
stripTypeMarks (MarkType start t end) = MarkType start (stripTypeMarks t) end

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
stripDeclMarks (MarkDecl _ d@(MarkDecl _ _ _) _) = stripDeclMarks d
stripDeclMarks (MarkDecl start d end) = MarkDecl start (stripDeclMarks d) end

stripStubMarks :: Stub -> Stub
stripStubMarks (StubDef i t c) = StubDef i (stripTypeMarks t) (map stripConstraintMarks c)
stripStubMarks (StubImplement p t c) = StubImplement p (stripTypeMarks t) (map stripConstraintMarks c)

stripModuleMarks :: Module -> Module
stripModuleMarks (Module x i d) = Module x i (map stripDeclMarks d)
