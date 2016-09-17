{-# LANGUAGE OverloadedStrings #-}

module ParseDeclTests (test) where

import Data.Text.Lazy (Text)

import Test.Hspec

import ParseDecl
import Syntax
import ParseUtils

import Data.Either (isLeft)

parseDecl :: Text -> Either ParseError Decl
parseDecl = fmap stripDeclMarks . fullParse decl

simpleIdent :: Letter -> Ident
simpleIdent l = Ident (Alpha LowerCase l) []

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

test :: Spec
test = describe "ParseDecl" $ do
  let typeIdentPure = TypeIdent P $ map (StartChar . Alpha LowerCase) [U, R, E]
  let typeVarDo = TypeVarIdent D [StartChar $ Alpha LowerCase O]

  describe "function defs" $ do
    describe "standard-notation defs" $ do
      let
        f = VarIdent (simpleIdent F)
          $ BodySlot
          $ EmptyTail

        g = VarIdent (simpleIdent G)
          $ BodySlot
          $ TailSlot
          $ EmptyTail

        h = VarIdent (simpleIdent H)
          $ BodySlot
          $ TailWord (simpleIdent I)
          $ TailSlot
          $ EmptyTail

        i = VarIdent (simpleIdent I)
          $ BodyWord (simpleIdent J)
          $ BodySlot
          $ EmptyTail

      it "parses minimal defs" $
        parseDecl "def f ( ) { }" `shouldBe` Right
          (DeclDef (PatVar f (TypeFunc TypeUnit TypePure TypeUnit)) (Func PatUnit Unit))

      it "rejects defs with no slots" $
        parseDecl "def f { }" `shouldSatisfy` isLeft

      it "rejects defs with reserved words as words" $
        parseDecl "def f do ( ) { }" `shouldSatisfy` isLeft

      it "parses defs with non-empty slots" $
        parseDecl "def f ( x : A ) { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc (simpleType A) TypePure TypeUnit))
            (Func (simplePatVar X) Unit)
          )

      it "rejects defs with arguments which lack type annotations" $
        parseDecl "def f ( x ) { }" `shouldSatisfy` isLeft

      it "parses defs with tuple patterns in a single slot" $
        parseDecl "def f ( x : A , y : B ) { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc (TypeTuple (simpleType A) (simpleType B)) TypePure TypeUnit))
            (Func (PatTuple (simplePatVar X) (simplePatVar Y)) Unit)
          )

      it "parses defs with multiple non-empty slots" $
        parseDecl "def g ( x : A ) ( y : B ) { }" `shouldBe` Right
          (DeclDef
            (PatVar g (TypeFunc (TypeTuple (simpleType A) (simpleType B)) TypePure TypeUnit))
            (Func (PatTuple (simplePatVar X) (simplePatVar Y)) Unit)
          )

      it "parses defs with words and slots interleaved" $
        parseDecl "def h ( x : A ) i ( y : B ) { }" `shouldBe` Right
          (DeclDef
            (PatVar h (TypeFunc (TypeTuple (simpleType A) (simpleType B)) TypePure TypeUnit))
            (Func (PatTuple (simplePatVar X) (simplePatVar Y)) Unit)
          )

      it "parses defs with consecutive words" $
        parseDecl "def i j ( ) { }" `shouldBe` Right
          (DeclDef
            (PatVar i (TypeFunc TypeUnit TypePure TypeUnit))
            (Func PatUnit Unit)
          )

      it "parses defs with non-empty interactions" $
        parseDecl "def f ( ) ! A | B { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit (TypeInters (simpleType A) (simpleType B)) TypeUnit))
            (Func PatUnit Unit)
          )

      it "parses defs with non-empty return types" $
        parseDecl "def f ( ) -> A { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit TypePure (simpleType A)))
            (Func PatUnit Unit)
          )

      it "rejects defs with unparenthesized tuples as return types" $
        parseDecl "def f ( ) -> A , B { }" `shouldSatisfy` isLeft

      it "parses defs with parenthesized tuples as return types" $
        parseDecl "def f ( ) -> ( A , B ) { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit TypePure (TypeTuple (simpleType A) (simpleType B))))
            (Func PatUnit Unit)
          )

      it "parses defs with curried functions as return types" $
        parseDecl "def f ( ) -> A -> B { }" `shouldBe` Right
          (DeclDef
            (PatVar
              f
              (TypeFunc
                TypeUnit
                TypePure
                (TypeFunc (simpleType A) TypePure (simpleType B))
              )
            )
            (Func PatUnit Unit)
          )

      it "parses defs with non-empty arguments, interactions, and return types" $
        parseDecl "def f ( x : A ) ! B | C -> D { }" `shouldBe` Right
          (DeclDef
            (PatVar
              f
              (TypeFunc
                (simpleType A)
                (TypeInters (simpleType B) (simpleType C))
                (simpleType D)
              )
            )
            (Func (simplePatVar X) Unit)
          )

      it "parses defs with non-empty bodies" $
        parseDecl "def f ( x : A ) { x }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc (simpleType A) TypePure TypeUnit))
            (Func (simplePatVar X) (simpleVarExpr X))
          )

      it "parses defs whose bodies have multiple statements" $
        parseDecl "def f ( x : A ) { y = x ; y }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc (simpleType A) TypePure TypeUnit))
            (Func (simplePatVar X) (Let (simplePatVar Y) (simpleVarExpr X) (simpleVarExpr Y)))
          )

    describe "dot-notation defs" $ do
      let
        f = DotVarIdent (simpleIdent F)
          $ EmptyTail

        g = DotVarIdent (simpleIdent G)
          $ TailSlot
          $ EmptyTail

        h = DotVarIdent (simpleIdent H)
          $ TailSlot
          $ TailSlot
          $ EmptyTail

        i = DotVarIdent (simpleIdent I)
          $ TailSlot
          $ TailWord (simpleIdent J)
          $ TailSlot
          $ EmptyTail

        j = DotVarIdent (simpleIdent J)
          $ TailWord  (simpleIdent K)
          $ TailSlot
          $ EmptyTail

      it "parses minimal defs" $
        parseDecl "def ( ) . f { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit TypePure TypeUnit))
            (Func PatUnit Unit)
          )

      it "rejects defs without a parenthesized receiver" $
        parseDecl "def . f { }" `shouldSatisfy` isLeft

      it "parses defs with non-empty receiver patterns" $
        parseDecl "def ( x : A ) . f { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc (simpleType A) TypePure TypeUnit))
            (Func (simplePatVar X) Unit)
          )

      it "parses defs with non-receiver slots" $
        parseDecl "def ( ) . g ( ) { }" `shouldBe` Right
          (DeclDef
            (PatVar g (TypeFunc (TypeTuple TypeUnit TypeUnit) TypePure TypeUnit))
            (Func (PatTuple PatUnit PatUnit) Unit)
          )

      it "parses defs with multiple consecutive non-receiver slots" $
        parseDecl "def ( x : A ) . h ( y : B ) ( z : C ) { }" `shouldBe` Right
          (DeclDef
            (PatVar
              h
              (TypeFunc
                (TypeTuple (simpleType A) $ TypeTuple (simpleType B) (simpleType C))
                TypePure
                TypeUnit
              )
            )
            (Func (PatTuple (simplePatVar X) $ PatTuple (simplePatVar Y) (simplePatVar Z)) Unit)
          )

      it "parses defs with non-empty receiver and non-receiver slots" $
        parseDecl "def ( x : A ) . g ( y : B ) { }" `shouldBe` Right
          (DeclDef
            (PatVar g (TypeFunc (TypeTuple (simpleType A) (simpleType B)) TypePure TypeUnit))
            (Func (PatTuple (simplePatVar X) (simplePatVar Y)) Unit)
          )

      it "parses defs with words and slots interleaved" $
        parseDecl "def ( ) . i ( ) j ( ) { }" `shouldBe` Right
          (DeclDef
            (PatVar
              i
              (TypeFunc
                (TypeTuple TypeUnit $ TypeTuple TypeUnit TypeUnit)
                TypePure
                TypeUnit
              )
            )
            (Func (PatTuple PatUnit $ PatTuple PatUnit PatUnit) Unit)
          )

      it "parses defs with consecutive words" $
        parseDecl "def ( ) . j k ( ) { }" `shouldBe` Right
          (DeclDef
            (PatVar j (TypeFunc (TypeTuple TypeUnit TypeUnit) TypePure TypeUnit))
            (Func (PatTuple PatUnit PatUnit) Unit)
          )

      it "parses defs with non-empty interactions" $
        parseDecl "def ( ) . f ! A | B { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit (TypeInters (simpleType A) (simpleType B)) TypeUnit))
            (Func PatUnit Unit)
          )

      it "parses defs with non-empty return types" $
        parseDecl "def ( ) . f -> A { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit TypePure (simpleType A)))
            (Func PatUnit Unit)
          )

  describe "type structures" $ do
    it "parses minimal structs" $
      parseDecl "type A { }" `shouldBe` Right (DeclTypeStruct (simpleTIdent A) [] [])

    it "parses structs with escaped names" $
      parseDecl "type `Pure` { }" `shouldBe` Right
        (DeclTypeStruct typeIdentPure [] [])

    it "parses structs with type parameters" $
      parseDecl "type A < b > < c > { }" `shouldBe` Right
        (DeclTypeStruct (simpleTIdent A) [simpleTypeVar B, simpleTypeVar C] [])

    it "parses structs with escaped type parameter names" $
      parseDecl "type A < `do` > { }" `shouldBe` Right
        (DeclTypeStruct (simpleTIdent A) [typeVarDo] [])

    it "parses structs with fields" $
      parseDecl "type A { x : B ; }" `shouldBe` Right
        (DeclTypeStruct (simpleTIdent A) [] [StructField (simpleIdent X) (simpleType B)])

    it "parses structs with escaped field names" $
      parseDecl "type A { `do` : B ; }" `shouldBe` Right
        (DeclTypeStruct
          (simpleTIdent A)
          []
          [StructField (Ident (Alpha LowerCase D) [StartChar $ Alpha LowerCase O]) (simpleType B)]
        )

    it "parses structs with multiple fields" $
      parseDecl "type A { x : B ; y : C ; }" `shouldBe` Right
        (DeclTypeStruct
          (simpleTIdent A)
          []
          [StructField (simpleIdent X) (simpleType B), StructField (simpleIdent Y) (simpleType C)]
        )

    it "parses structs with fields typed by unparenthesized tuples" $
      parseDecl "type A { x : B , C ; }" `shouldBe` Right
        (DeclTypeStruct
          (simpleTIdent A)
          []
          [StructField (simpleIdent X) (TypeTuple (simpleType B) (simpleType C))]
        )

    it "parses structs with cases" $
      parseDecl "type A { case B { } }" `shouldBe` Right
        (DeclTypeStruct (simpleTIdent A) [] [StructCase (simpleTIdent B) []])

    it "parses structs with cases with fields" $
      parseDecl "type A { case B { x : C ; } }" `shouldBe` Right
        (DeclTypeStruct
          (simpleTIdent A)
          []
          [StructCase (simpleTIdent B) [StructField (simpleIdent X) (simpleType C)]]
        )

    it "parses structs with semicolon-notation empty cases" $
      parseDecl "type A { case B ; }" `shouldBe` Right
        (DeclTypeStruct (simpleTIdent A) [] [StructCase (simpleTIdent B) []])

    it "rejects semicolon-notation empty structs" $
      parseDecl "type A ;" `shouldSatisfy` isLeft

    it "rejects cases with type parameters" $
      parseDecl "type A { case B < t > { } }" `shouldSatisfy` isLeft

    it "parses structs with multiple cases" $
      parseDecl "type A { case B { } case C ; case D { } }" `shouldBe` Right
        (DeclTypeStruct
          (simpleTIdent A)
          []
          [ StructCase (simpleTIdent B) []
          , StructCase (simpleTIdent C) []
          , StructCase (simpleTIdent D) []
          ]
        )

    it "parses structs with cases and fields interleaved" $
      parseDecl "type A { x : B ; case C { } y : D ; case E ; z : F ; }" `shouldBe`
        Right (DeclTypeStruct
          (simpleTIdent A)
          []
          [ StructField (simpleIdent X) (simpleType B)
          , StructCase (simpleTIdent C) []
          , StructField (simpleIdent Y) (simpleType D)
          , StructCase (simpleTIdent E) []
          , StructField (simpleIdent Z) (simpleType F)
          ]
        )

    it "parses nested cases" $
      parseDecl "type A { case B { case C { } } }" `shouldBe` Right
        (DeclTypeStruct
          (simpleTIdent A)
          []
          [StructCase (simpleTIdent B) [StructCase (simpleTIdent C) []]]
        )

    it "parses cases with escaped names" $
      parseDecl "type A { case `Pure` { } }" `shouldBe` Right
        (DeclTypeStruct
          (simpleTIdent A)
          []
          [StructCase typeIdentPure []]
        )

  describe "protocols" $ do
    it "parses minimal protocols" $
      parseDecl "protocol A < t > { }" `shouldBe` Right
        (DeclProtocol (simpleTIdent A) (simpleTypeVar T) [])

    it "parses protocols with escaped names" $
      parseDecl "protocol `Pure` < t > { }" `shouldBe` Right
        (DeclProtocol typeIdentPure (simpleTypeVar T) [])

    it "parses protocols with escaped type variable names" $
      parseDecl "protocol A < `do` > { }" `shouldBe` Right
        (DeclProtocol (simpleTIdent A) typeVarDo [])

    -- def stubs are implemented with the same logic as ordinary defs, so it's not crucial to test
    -- them as thoroughly as defs.

    it "parses protocols with minimal def stubs" $
      parseDecl "protocol A < t > { def f ( ) ; }" `shouldBe` Right
        (DeclProtocol
          (simpleTIdent A)
          (simpleTypeVar T)
          [StubDef (simpleVar F) (TypeFunc TypeUnit TypePure TypeUnit)]
        )

    it "parses protocols with nontrivial def stubs" $
      parseDecl "protocol A < t > { def f ( A ) g ( B ) ! C -> D ; }" `shouldBe` Right
        (DeclProtocol
          (simpleTIdent A)
          (simpleTypeVar T)
          [StubDef
            (VarIdent (simpleIdent F) $ BodySlot $ TailWord (simpleIdent G) $ TailSlot $ EmptyTail)
            (TypeFunc (TypeTuple (simpleType A) (simpleType B)) (simpleType C) (simpleType D))
          ]
        )

    it "parses protocols with minimal dot-notation def stubs" $
      parseDecl "protocol A < t > { def ( ) . f ; }" `shouldBe` Right
        (DeclProtocol
          (simpleTIdent A)
          (simpleTypeVar T)
          [StubDef (DotVarIdent (simpleIdent F) $ EmptyTail) (TypeFunc TypeUnit TypePure TypeUnit)]
        )

    it "parses protocols with nontrivial dot-notation def stubs" $
      parseDecl "protocol A < t > { def ( A ) . f ( B ) g ( C ) ! D -> E ; }" `shouldBe` Right
        (DeclProtocol
          (simpleTIdent A)
          (simpleTypeVar T)
          [StubDef
            ( DotVarIdent (simpleIdent F)
            $ TailSlot
            $ TailWord (simpleIdent G)
            $ TailSlot
            $ EmptyTail
            )
            (TypeFunc
              (TypeTuple (simpleType A) $ TypeTuple (simpleType B) (simpleType C))
              (simpleType D)
              (simpleType E)
            )
          ]
        )

    it "parses protocols with implement stubs" $
      parseDecl "protocol A < t > { implement B < C > ; }" `shouldBe` Right
        (DeclProtocol
          (simpleTIdent A)
          (simpleTypeVar T)
          [StubImplement (simpleTIdent B) (simpleType C)]
        )

    it "parses protocols with multiple stubs" $
      parseDecl "protocol A < t > { def f ( ) ; def g ( ) ; }" `shouldBe` Right
        (DeclProtocol
          (simpleTIdent A)
          (simpleTypeVar T)
          [ StubDef (simpleVar F) (TypeFunc TypeUnit TypePure TypeUnit)
          , StubDef (simpleVar G) (TypeFunc TypeUnit TypePure TypeUnit)
          ]
        )
