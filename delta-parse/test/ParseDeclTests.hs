{-# LANGUAGE OverloadedStrings #-}

module ParseDeclTests (test) where

import Data.Text.Lazy (Text)

import Test.Hspec

import ParseDecl
import Delta.Structures.Syntax
import StripMarks (stripDeclMarks)
import ParseUtils
import SyntaxUtils

import Data.Either (isLeft)

parseDecl :: Text -> Either ParseError Decl
parseDecl = fmap stripDeclMarks . fullParse decl

test :: Spec
test = describe "ParseDecl" $ do
  let typeVarDo = typeVarIdentText $ TypeVarIdent D [StartChar $ Alpha LowerCase O]

  describe "function defs" $ do
    describe "standard-notation defs" $ do
      let
        f = varIdentText
          $ VarIdent (simpleIdent F)
          $ BodySlot
          $ EmptyTail

        g = varIdentText
          $ VarIdent (simpleIdent G)
          $ BodySlot
          $ TailSlot
          $ EmptyTail

        h = varIdentText
          $ VarIdent (simpleIdent H)
          $ BodySlot
          $ TailWord (simpleIdent I)
          $ TailSlot
          $ EmptyTail

        i = varIdentText
          $ VarIdent (simpleIdent I)
          $ BodyWord (simpleIdent J)
          $ BodySlot
          $ EmptyTail

      it "parses minimal defs" $
        parseDecl "def f ( ) { }" `shouldBe` Right
          (DeclDef (PatVar f (TypeFunc TypeUnit TypePure TypeUnit)) [] (Func PatUnit Unit))

      it "rejects defs with no slots" $
        parseDecl "def f { }" `shouldSatisfy` isLeft

      it "rejects defs with reserved words as words" $
        parseDecl "def f do ( ) { }" `shouldSatisfy` isLeft

      it "parses defs with non-empty slots" $
        parseDecl "def f ( x : A ) { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc (simpleType A) TypePure TypeUnit)) []
            (Func (simplePatVar X) Unit)
          )

      it "rejects defs with arguments which lack type annotations" $
        parseDecl "def f ( x ) { }" `shouldSatisfy` isLeft

      it "parses defs with tuple patterns in a single slot" $
        parseDecl "def f ( x : A , y : B ) { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc (TypeTuple (simpleType A) (simpleType B)) TypePure TypeUnit)) []
            (Func (PatTuple (simplePatVar X) (simplePatVar Y)) Unit)
          )

      it "parses defs with multiple non-empty slots" $
        parseDecl "def g ( x : A ) ( y : B ) { }" `shouldBe` Right
          (DeclDef
            (PatVar g (TypeFunc (TypeTuple (simpleType A) (simpleType B)) TypePure TypeUnit)) []
            (Func (PatTuple (simplePatVar X) (simplePatVar Y)) Unit)
          )

      it "parses defs with words and slots interleaved" $
        parseDecl "def h ( x : A ) i ( y : B ) { }" `shouldBe` Right
          (DeclDef
            (PatVar h (TypeFunc (TypeTuple (simpleType A) (simpleType B)) TypePure TypeUnit)) []
            (Func (PatTuple (simplePatVar X) (simplePatVar Y)) Unit)
          )

      it "parses defs with consecutive words" $
        parseDecl "def i j ( ) { }" `shouldBe` Right
          (DeclDef
            (PatVar i (TypeFunc TypeUnit TypePure TypeUnit)) []
            (Func PatUnit Unit)
          )

      it "parses defs with non-empty interactions" $
        parseDecl "def f ( ) ! A | B { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit (TypeInters (simpleType A) (simpleType B)) TypeUnit)) []
            (Func PatUnit Unit)
          )

      it "parses defs with non-empty return types" $
        parseDecl "def f ( ) -> A { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit TypePure (simpleType A))) []
            (Func PatUnit Unit)
          )

      it "rejects defs with unparenthesized tuples as return types" $
        parseDecl "def f ( ) -> A , B { }" `shouldSatisfy` isLeft

      it "parses defs with parenthesized tuples as return types" $
        parseDecl "def f ( ) -> ( A , B ) { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit TypePure (TypeTuple (simpleType A) (simpleType B)))) []
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
            []
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
            []
            (Func (simplePatVar X) Unit)
          )

      it "parses defs with non-empty bodies" $
        parseDecl "def f ( x : A ) { x }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc (simpleType A) TypePure TypeUnit)) []
            (Func (simplePatVar X) (simpleVarExpr X))
          )

      it "parses defs whose bodies have multiple statements" $
        parseDecl "def f ( x : A ) { y = x ; y }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc (simpleType A) TypePure TypeUnit)) []
            (Func (simplePatVar X) (Let (simplePatVar Y) (simpleVarExpr X) (simpleVarExpr Y)))
          )

      it "parses defs with constraints" $
        parseDecl "def f ( ) where A < B > { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit TypePure TypeUnit))
            [ConstraintImplement (Path [] $ simpleTIdent A) (simpleType B)]
            (Func PatUnit Unit)
          )

      it "parses defs with multiple constraints" $
        parseDecl "def f ( ) where A < B > , C < D > { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit TypePure TypeUnit))
            [ ConstraintImplement (Path [] $ simpleTIdent A) (simpleType B)
            , ConstraintImplement (Path [] $ simpleTIdent C) (simpleType D)
            ]
            (Func PatUnit Unit)
          )

      it "parses defs with multiple constraints with a trailing comma" $
        parseDecl "def f ( ) where A < B > , C < D > , { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit TypePure TypeUnit))
            [ ConstraintImplement (Path [] $ simpleTIdent A) (simpleType B)
            , ConstraintImplement (Path [] $ simpleTIdent C) (simpleType D)
            ]
            (Func PatUnit Unit)
          )

      it "parses defs with constraints with qualified names" $
        parseDecl "def f ( ) where A :: B < C > { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit TypePure TypeUnit))
            [ConstraintImplement (Path [simpleModule A] $ simpleTIdent B) (simpleType C)]
            (Func PatUnit Unit)
          )

      it "parses defs with constraints with escaped names" $
        parseDecl "def f ( ) where ` Pure ` < A > { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit TypePure TypeUnit))
            [ConstraintImplement (Path [] $ typeIdentPure) (simpleType A)]
            (Func PatUnit Unit)
          )

    describe "dot-notation defs" $ do
      let
        f = varIdentText
          $ DotVarIdent (simpleIdent F)
          $ EmptyTail

        g = varIdentText
          $ DotVarIdent (simpleIdent G)
          $ TailSlot
          $ EmptyTail

        h = varIdentText
          $ DotVarIdent (simpleIdent H)
          $ TailSlot
          $ TailSlot
          $ EmptyTail

        i = varIdentText
          $ DotVarIdent (simpleIdent I)
          $ TailSlot
          $ TailWord (simpleIdent J)
          $ TailSlot
          $ EmptyTail

        j = varIdentText
          $ DotVarIdent (simpleIdent J)
          $ TailWord  (simpleIdent K)
          $ TailSlot
          $ EmptyTail

      it "parses minimal defs" $
        parseDecl "def ( ) . f { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit TypePure TypeUnit)) []
            (Func PatUnit Unit)
          )

      it "rejects defs without a parenthesized receiver" $
        parseDecl "def . f { }" `shouldSatisfy` isLeft

      it "parses defs with non-empty receiver patterns" $
        parseDecl "def ( x : A ) . f { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc (simpleType A) TypePure TypeUnit)) []
            (Func (simplePatVar X) Unit)
          )

      it "parses defs with non-receiver slots" $
        parseDecl "def ( ) . g ( ) { }" `shouldBe` Right
          (DeclDef
            (PatVar g (TypeFunc (TypeTuple TypeUnit TypeUnit) TypePure TypeUnit)) []
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
            []
            (Func (PatTuple (simplePatVar X) $ PatTuple (simplePatVar Y) (simplePatVar Z)) Unit)
          )

      it "parses defs with non-empty receiver and non-receiver slots" $
        parseDecl "def ( x : A ) . g ( y : B ) { }" `shouldBe` Right
          (DeclDef
            (PatVar g (TypeFunc (TypeTuple (simpleType A) (simpleType B)) TypePure TypeUnit)) []
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
            []
            (Func (PatTuple PatUnit $ PatTuple PatUnit PatUnit) Unit)
          )

      it "parses defs with consecutive words" $
        parseDecl "def ( ) . j k ( ) { }" `shouldBe` Right
          (DeclDef
            (PatVar j (TypeFunc (TypeTuple TypeUnit TypeUnit) TypePure TypeUnit)) []
            (Func (PatTuple PatUnit PatUnit) Unit)
          )

      it "parses defs with non-empty interactions" $
        parseDecl "def ( ) . f ! A | B { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit (TypeInters (simpleType A) (simpleType B)) TypeUnit)) []
            (Func PatUnit Unit)
          )

      it "parses defs with non-empty return types" $
        parseDecl "def ( ) . f -> A { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit TypePure (simpleType A))) []
            (Func PatUnit Unit)
          )

      it "parses defs with constraints" $
        parseDecl "def ( ) . f where A < B > { }" `shouldBe` Right
          (DeclDef
            (PatVar f (TypeFunc TypeUnit TypePure TypeUnit))
            [ConstraintImplement (Path [] $ simpleTIdent A) (simpleType B)]
            (Func PatUnit Unit)
          )

    describe "operator-notation defs" $ do
      it "parses binary-operator-notation defs" $
        -- Testing with '-', not some other operator, because it could theoretically conflict with
        -- the '->' return type syntax.
        parseDecl "def ( x : A ) - ( y : B ) ! C -> D { }" `shouldBe` Right
          (DeclDef
            (PatVar
              (varIdentText $ OperatorIdent OpSub)
              (TypeFunc (TypeTuple (simpleType A) (simpleType B)) (simpleType C) (simpleType D))
            )
            []
            (Func (PatTuple (simplePatVar X) (simplePatVar Y)) Unit)
          )

      it "parses prefix-operator-notation defs" $
        parseDecl "def - ( x : A ) ! B -> C { }" `shouldBe` Right
          (DeclDef
            (PatVar
              (varIdentText $ PrefixOperatorIdent OpNegate)
              (TypeFunc (simpleType A) (simpleType B) (simpleType C))
            )
            []
            (Func (simplePatVar X) Unit)
          )

    describe "non-function defs" $ do
      it "parses simple defs" $
        parseDecl "x : A = y ;" `shouldBe` Right
          (DeclDef (PatVar (simpleVar X) (simpleType A)) [] (simpleVarExpr Y))

      it "parses defs with nontrivial patterns" $
        parseDecl "( ) , ( x : A , _ : B ) , y : C = z ;" `shouldBe` Right
          (DeclDef
            (PatTuple PatUnit
            $ PatTuple (PatTuple (PatVar (simpleVar X) (simpleType A)) (PatIgnore (simpleType B)))
            $ PatVar (simpleVar Y) (simpleType C)
            )
            []
            (simpleVarExpr Z)
          )

      it "parses defs with constraints" $
        parseDecl "x : A where B < C > , D < E > = y ;" `shouldBe` Right
          (DeclDef
            (PatVar (simpleVar X) (simpleType A))
            [ ConstraintImplement (Path [] $ simpleTIdent B) (simpleType C)
            , ConstraintImplement (Path [] $ simpleTIdent D) (simpleType E)
            ]
            (simpleVarExpr Y)
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
          [StructField (identText $ Ident (Alpha LowerCase D) [StartChar $ Alpha LowerCase O]) (simpleType B)]
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
          [StubDef (simpleVar F) (TypeFunc TypeUnit TypePure TypeUnit) []]
        )

    it "parses protocols with nontrivial def stubs" $
      parseDecl "protocol A < t > { def f ( A ) g ( B ) ! C -> D ; }" `shouldBe` Right
        (DeclProtocol
          (simpleTIdent A)
          (simpleTypeVar T)
          [StubDef
            (varIdentText $ VarIdent (simpleIdent F) $ BodySlot $ TailWord (simpleIdent G) $ TailSlot $ EmptyTail)
            (TypeFunc (TypeTuple (simpleType A) (simpleType B)) (simpleType C) (simpleType D)) []
          ]
        )

    it "parses protocols with minimal dot-notation def stubs" $
      parseDecl "protocol A < t > { def ( ) . f ; }" `shouldBe` Right
        (DeclProtocol
          (simpleTIdent A)
          (simpleTypeVar T)
          [StubDef (varIdentText $ DotVarIdent (simpleIdent F) $ EmptyTail) (TypeFunc TypeUnit TypePure TypeUnit) []]
        )

    it "parses protocols with nontrivial dot-notation def stubs" $
      parseDecl "protocol A < t > { def ( A ) . f ( B ) g ( C ) ! D -> E ; }" `shouldBe` Right
        (DeclProtocol
          (simpleTIdent A)
          (simpleTypeVar T)
          [StubDef
            ( varIdentText
            $ DotVarIdent (simpleIdent F)
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
            []
          ]
        )

    it "parses protocols with nontrivial operator-notation def stubs" $
      parseDecl "protocol A < t > { def ( B ) - ( C ) ! D -> E ; }" `shouldBe` Right
        (DeclProtocol
          (simpleTIdent A)
          (simpleTypeVar T)
          [StubDef
            (varIdentText $ OperatorIdent OpSub)
            (TypeFunc (TypeTuple (simpleType B) (simpleType C)) (simpleType D) (simpleType E)) []
          ]
        )

    it "parses protocols with def stubs with constraints" $
      parseDecl "protocol A < t > { def f ( ) where A < B > ; }" `shouldBe` Right
        (DeclProtocol
          (simpleTIdent A)
          (simpleTypeVar T)
          [StubDef
            (simpleVar F)
            (TypeFunc TypeUnit TypePure TypeUnit)
            [ConstraintImplement (Path [] $ simpleTIdent A) (simpleType B)]
          ]
        )

    it "parses protocols with non-function def stubs" $
      parseDecl "protocol A < t > { x : B ; }" `shouldBe` Right
        (DeclProtocol (simpleTIdent A) (simpleTypeVar T) [StubDef (simpleVar X) (simpleType B) []])

    it "parses protocols with non-function def stubs with constraints" $
      parseDecl "protocol A < t > { x : B where C < D > ; }" `shouldBe` Right
        (DeclProtocol
          (simpleTIdent A)
          (simpleTypeVar T)
          [StubDef
            (simpleVar X)
            (simpleType B)
            [ConstraintImplement (Path [] $ simpleTIdent C) (simpleType D)]
          ]
        )

    it "parses protocols with implement stubs" $
      parseDecl "protocol A < t > { implement B < C > ; }" `shouldBe` Right
        (DeclProtocol
          (simpleTIdent A)
          (simpleTypeVar T)
          [StubImplement (Path [] $ simpleTIdent B) (simpleType C) []]
        )

    it "parses protocols with implement stubs with qualified names" $
      parseDecl "protocol A < t > { implement B :: C < D > ; }" `shouldBe` Right
        (DeclProtocol
          (simpleTIdent A)
          (simpleTypeVar T)
          [StubImplement (Path [simpleModule B] $ simpleTIdent C) (simpleType D) []]
        )

    it "parses protocols with implement stubs with constraints" $
      parseDecl "protocol A < t > { implement B < C > where D < E > ; }" `shouldBe` Right
        (DeclProtocol
          (simpleTIdent A)
          (simpleTypeVar T)
          [StubImplement
            (Path [] $ simpleTIdent B)
            (simpleType C)
            [ConstraintImplement (Path [] $ simpleTIdent D) (simpleType E)]
          ]
        )

    it "parses protocols with multiple stubs" $
      parseDecl "protocol A < t > { def f ( ) ; def g ( ) ; }" `shouldBe` Right
        (DeclProtocol
          (simpleTIdent A)
          (simpleTypeVar T)
          [ StubDef (simpleVar F) (TypeFunc TypeUnit TypePure TypeUnit) []
          , StubDef (simpleVar G) (TypeFunc TypeUnit TypePure TypeUnit) []
          ]
        )

  describe "protocol implementations" $ do
    it "parses minimal implementations" $
      parseDecl "implement A < B > { }" `shouldBe` Right
        (DeclImplement (Path [] $ simpleTIdent A) (simpleType B) [] [])

    it "parses implementations with qualified names" $
      parseDecl "implement A :: B < C > { }" `shouldBe` Right
        (DeclImplement (Path [simpleModule A] $ simpleTIdent B) (simpleType C) [] [])

    it "parses implementations with escaped names" $
      parseDecl "implement ` Pure ` < A > { }" `shouldBe` Right
        (DeclImplement (Path [] $ typeIdentPure) (simpleType A) [] [])

    it "parses implementations for nontrivial types" $
      parseDecl "implement A < B , C > { }" `shouldBe` Right
        (DeclImplement (Path [] $ simpleTIdent A) (TypeTuple (simpleType B) (simpleType C)) [] [])

    it "parses implementations with constraints" $
      parseDecl "implement A < B > where C < D > { }" `shouldBe` Right
        (DeclImplement
          (Path [] $ simpleTIdent A)
          (simpleType B)
          [ConstraintImplement (Path [] $ simpleTIdent C) (simpleType D)]
          []
        )

    -- defs in implementations are implemented with the same logic as ordinary defs, so it's not
    -- crucial to test them as thoroughly as top-level def declarations.

    it "parses implementations with minimal defs" $
      parseDecl "implement A < B > { def f ( ) { } }" `shouldBe` Right
        (DeclImplement
          (Path [] $ simpleTIdent A)
          (simpleType B) []
          [ ( PatVar (simpleVar F) (TypeFunc TypeUnit TypePure TypeUnit), []
            , Func PatUnit Unit
            )
          ]
        )

    it "parses implementations with nontrivial def signatures" $
      parseDecl "implement A < B > { def f ( x : A ) g ( y : B ) ! C -> D { } }" `shouldBe` Right
        (DeclImplement
          (Path [] $ simpleTIdent A)
          (simpleType B) []
          [ ( PatVar
              (varIdentText $ VarIdent (simpleIdent F) $ BodySlot $ TailWord (simpleIdent G) $ TailSlot $ EmptyTail)
              (TypeFunc (TypeTuple (simpleType A) (simpleType B)) (simpleType C) (simpleType D))
            , []
            , Func (PatTuple (simplePatVar X) (simplePatVar Y)) Unit
            )
          ]
        )

    it "parses implementations with minimal dot-notation defs" $
      parseDecl "implement A < B > { def ( ) . f { } }" `shouldBe` Right
        (DeclImplement
          (Path [] $ simpleTIdent A)
          (simpleType B) []
          [ ( PatVar (varIdentText $ DotVarIdent (simpleIdent F) $ EmptyTail) (TypeFunc TypeUnit TypePure TypeUnit), []
            , Func PatUnit Unit
            )
          ]
        )

    it "parses implementations with nontrivial dot-notation def" $
      parseDecl "implement A < B > { def ( x : A ) . f ( y : B ) g ( z : C ) ! D -> E { } }" `shouldBe` Right
          (DeclImplement
            (Path [] $ simpleTIdent A)
            (simpleType B) []
            [ ( PatVar
                ( varIdentText
                $ DotVarIdent (simpleIdent F)
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
              , []
              , Func (PatTuple (simplePatVar X) $ PatTuple (simplePatVar Y) (simplePatVar Z)) Unit
              )
            ]
          )

    it "parses implementations with nontrivial operator-notation defs" $
      parseDecl "implement A < B > { def ( x : C ) - ( y : D ) ! E -> F { } }" `shouldBe` Right
        (DeclImplement
          (Path [] $ simpleTIdent A)
          (simpleType B) []
          [ ( PatVar
              (varIdentText $ OperatorIdent OpSub)
              (TypeFunc (TypeTuple (simpleType C) (simpleType D)) (simpleType E) (simpleType F))
            , []
            , Func (PatTuple (simplePatVar X) (simplePatVar Y)) Unit
            )
          ]
        )

    it "parses implementations with non-function defs" $
      parseDecl "implement A < B > { x : C = y ; }" `shouldBe` Right
        (DeclImplement
          (Path [] $ simpleTIdent A)
          (simpleType B) []
          [(PatVar (simpleVar X) (simpleType C), [], simpleVarExpr Y)]
        )

    it "parses implementations with defs with bodies" $
      parseDecl "implement A < B > { def f ( x : C ) -> C { x } }" `shouldBe` Right
        (DeclImplement
          (Path [] $ simpleTIdent A)
          (simpleType B) []
          [ ( PatVar (simpleVar F) (TypeFunc (simpleType C) TypePure (simpleType C)), []
            , (Func (simplePatVar X) (simpleVarExpr X))
            )
          ]
        )

    it "parses implementations with defs with constraints" $
      parseDecl "implement A < B > { def f ( ) where C < D > { } }" `shouldBe` Right
        (DeclImplement
          (Path [] $ simpleTIdent A)
          (simpleType B)
          []
          [ ( PatVar (simpleVar F) (TypeFunc TypeUnit TypePure TypeUnit)
            , [ConstraintImplement (Path [] $ simpleTIdent C) (simpleType D)]
            , Func PatUnit Unit
            )
          ]
        )

    it "parses implementations with multiple defs" $
      parseDecl "implement A < B > { def f ( ) { } def g ( ) { } }" `shouldBe` Right
        (DeclImplement
          (Path [] $ simpleTIdent A)
          (simpleType B) []
          [ ( PatVar (simpleVar F) (TypeFunc TypeUnit TypePure TypeUnit), []
            , Func PatUnit Unit
            )
          , ( PatVar (simpleVar G) (TypeFunc TypeUnit TypePure TypeUnit), []
            , Func PatUnit Unit
            )
          ]
        )

  describe "interactions" $ do
    it "parses minimal interactions" $
      parseDecl "interaction A { }" `shouldBe` Right (DeclInteraction (simpleTIdent A) [] [])

    it "parses interactions with escaped names" $
      parseDecl "interaction `Pure` { }" `shouldBe` Right (DeclInteraction typeIdentPure [] [])

    it "parses interactions with type parameters" $
      parseDecl "interaction A < x > < y > { }" `shouldBe` Right
        (DeclInteraction (simpleTIdent A) [simpleTypeVar X, simpleTypeVar Y] [])

    it "parses interactions with type parameters with escaped names" $
      parseDecl "interaction A < ` do ` > { }" `shouldBe` Right
        (DeclInteraction (simpleTIdent A) [typeVarDo] [])

    it "parses interactions with messages" $
      parseDecl "interaction A { message f ( B ) -> C ; }" `shouldBe` Right
        (DeclInteraction (simpleTIdent A) [] [(simpleIdent F, simpleType B, simpleType C)])

    it "parses interactions with messages with implicit argument type unit" $
      parseDecl "interaction A { message f ( ) -> B ; }" `shouldBe` Right
        (DeclInteraction (simpleTIdent A) [] [(simpleIdent F, TypeUnit, simpleType B)])

    it "parses interactions with messages with escaped names" $
      parseDecl "interaction A { message ` do ` ( B ) -> C ; }" `shouldBe` Right
        (DeclInteraction
          (simpleTIdent A)
          []
          [(identText $ Ident (Alpha LowerCase D) [StartChar $ Alpha LowerCase O], simpleType B, simpleType C)]
        )

    it "rejects messages without explicit return types" $
      parseDecl "interaction A { message f ( B ) ; }" `shouldSatisfy` isLeft

    it "rejects messages which return unparenthesized tuples" $
      parseDecl "interaction A { message f ( B ) -> C , D ; }" `shouldSatisfy` isLeft

    it "rejects messages which make use of free-form function call syntax" $
      parseDecl "interaction A { message f ( B ) g ( C ) -> D ; }" `shouldSatisfy` isLeft

    it "parses interactions with multiple messages" $
      parseDecl "interaction A { message f ( B ) -> C ; message g ( D ) -> E ; }" `shouldBe` Right
        (DeclInteraction
          (simpleTIdent A)
          []
          [(simpleIdent F, simpleType B, simpleType C), (simpleIdent G, simpleType D, simpleType E)]
        )
