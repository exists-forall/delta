{-# LANGUAGE OverloadedStrings #-}

module ParseExprTests (test) where

import Data.Text.Lazy (Text)

import Test.Hspec

import ParseExpr
import Syntax
import ParseUtils

import Data.Either (isLeft)

simpleIdent :: Letter -> Ident
simpleIdent l = Ident (Alpha LowerCase l) []

simpleUpperIdent :: Letter -> Ident
simpleUpperIdent l = Ident (Alpha UpperCase l) []

parseExpr :: Text -> Either ParseError Expr
parseExpr = fmap stripMarks . fullParse expr

test :: Spec
test = describe "ParseExpr" $ do
  let
    m = ModuleIdent M []
    n = ModuleIdent N []

    -- Simple variables

    x = Var
      $ Path []
      $ VarIdent (simpleIdent X)
      $ BodySlot
      $ EmptyTail

    y = Var
      $ Path [m, n]
      $ VarIdent (simpleIdent Y)
      $ BodySlot
      $ EmptyTail

    -- Regular-notation functions

    f = Var
      $ Path []
      $ VarIdent (simpleIdent F)
      $ BodySlot
      $ EmptyTail

    g = Var
      $ Path [m, n]
      $ VarIdent (simpleUpperIdent G)
      $ BodySlot
      $ EmptyTail

    h = Var
      $ Path []
      $ VarIdent (simpleIdent H)
      $ BodySlot
      $ TailSlot
      $ EmptyTail

    i = Var
      $ Path []
      $ VarIdent (simpleIdent I)
      $ BodySlot
      $ TailWord (simpleIdent J)
      $ TailSlot
      $ EmptyTail

    k = Var
      $ Path []
      $ VarIdent (simpleIdent K)
      $ BodyWord (simpleIdent L)
      $ BodySlot
      $ EmptyTail

    -- Dot-notation functions

    a = Var
      $ Path []
      $ DotVarIdent (simpleIdent A)
      $ EmptyTail

    b = Var
      $ Path [m, n]
      $ DotVarIdent (simpleUpperIdent B)
      $ EmptyTail

    c = Var
      $ Path []
      $ DotVarIdent (simpleIdent C)
      $ TailSlot
      $ EmptyTail

    d = Var
      $ Path []
      $ DotVarIdent (simpleIdent D)
      $ TailSlot
      $ TailSlot
      $ EmptyTail

    e = Var
      $ Path []
      $ DotVarIdent (simpleIdent E)
      $ TailSlot
      $ TailWord (simpleIdent F)
      $ TailSlot
      $ EmptyTail

    add = Var $ Path [] $ OperatorIdent OpAdd
    mul = Var $ Path [] $ OperatorIdent OpMul
    equ = Var $ Path [] $ OperatorIdent OpEqu
    logicOr = Var $ Path [] $ OperatorIdent OpOr

    xPat = flip PatVar Nothing
         $ VarIdent (simpleIdent X)
         $ BodySlot
         $ EmptyTail

  describe "expr" $ do
    it "parses simple identifiers" $
      parseExpr "x" `shouldBe` Right x

    it "parses qualified identifiers" $
      parseExpr "M::N :: y" `shouldBe` Right y

    it "parses simple escaped identifiers" $
      parseExpr "M :: N :: ` y `" `shouldBe` Right y

    it "parses escaped identifiers with a single slot" $
      parseExpr "` x () `" `shouldBe` Right x

    it "parses escaped identifiers with multiple words and slots" $
      parseExpr "` i ( ) j ( ) `" `shouldBe` Right i

    it "parses escaped dot-function-style identifiers" $
      parseExpr "` . e ( ) f ( ) `" `shouldBe` Right e

    it "parses escaped binary operators" $
      parseExpr "` + `" `shouldBe` Right (Var (Path [] (OperatorIdent OpAdd)))

    it "parses unsigned integer literals" $
      parseExpr "42" `shouldBe` Right (LitUInt 42)

    it "parses unsigned integer literals starting with zero" $
      parseExpr "042" `shouldBe` Right (LitUInt 42)

    it "parses simple strings" $
      parseExpr "\"foo bar\"" `shouldBe` Right (LitString (map Char "foo bar"))

    it "parses strings containing unicode characters" $
      parseExpr "\"π ≈ 3.1415\"" `shouldBe` Right (LitString (map Char "π ≈ 3.1415"))

    it "parses strings containing non-escaped newlines and tabs" $
      parseExpr "\"foo\nbar\tbaz\"" `shouldBe` Right (LitString (map Char "foo\nbar\tbaz"))

    it "parses strings containing simple escape sequences" $
      parseExpr "\"foo\\nbar\\tbaz\\\"biz\\\\buz\\rquux\"" `shouldBe`
        Right (LitString $ map Char "foo\nbar\tbaz\"biz\\buz\rquux")

    it "parses strings containing arbitrary unicode escape sequences" $
      parseExpr "\"\\u{3c0} \\u{2248} 3.1415\"" `shouldBe`
        Right (LitString $ map Char "π ≈ 3.1415")

    it "parses strings containing interpolated expressions" $
      parseExpr "\"x=\\(x)!\"" `shouldBe`
        Right (LitString [Char 'x', Char '=', Interpolate x, Char '!'])

    it "parses nullary function calls" $
      parseExpr "f()" `shouldBe` Right (Call f Unit)

    it "parses unary function calls" $
      parseExpr "f(x)" `shouldBe` Right (Call f x)

    it "parses function calls with naked string arguments" $
      parseExpr "f \"x\"" `shouldBe` Right (Call f (LitString [Char 'x']))

    it "parses function calls with naked function arguments with no argument list" $
      parseExpr "f { x }" `shouldBe` Right (Call f (Func PatIgnore x))

    it "parses function calls with naked function arguments with argument lists" $
      parseExpr "f | x | { x }" `shouldBe` Right (Call f (Func xPat x))

    it "parses function calls with naked 'do'-notation function arguments with no argument list" $
      parseExpr "f do x" `shouldBe` Right (Call f (Func PatIgnore x))

    it "parses function calls with naked 'do'-notation function arguments with argument lists" $
      parseExpr "f | x | do x" `shouldBe` Right (Call f (Func xPat x))

    it "parses qualified function calls" $
      parseExpr "M :: N::G()" `shouldBe` Right (Call g Unit)

    it "parses nested function calls" $
      parseExpr "f(  M::  N    :: G ( x ))" `shouldBe` Right (Call f (Call g x))

    it "parses function calls with multiple slots" $
      parseExpr "h()   (  )" `shouldBe` Right (Call h (Tuple Unit Unit))

    it "parses function calls with multiple filled slots" $
      parseExpr "h (x )( M::N::y)" `shouldBe` Right (Call h (Tuple x y))

    it "parses function calls with words and slots interleaved" $
      parseExpr "i ()j (  )" `shouldBe` Right (Call i (Tuple Unit Unit))

    it "parses function calls with consecutive words" $
      parseExpr "k l ( )" `shouldBe` Right (Call k Unit)

    it "parses function calls with words and filled slots interleaved" $
      parseExpr "i (  x) j(M::N :: y)" `shouldBe` Right (Call i (Tuple x y))

    it "parses dot function calls" $
      parseExpr "x.a" `shouldBe` Right (Call a x)

    it "parses qualified dot function calls" $
      parseExpr "x . M ::N::  B" `shouldBe` Right (Call b x)

    it "parses dot function calls with slots" $
      parseExpr "x.c ( )" `shouldBe` Right (Call c (Tuple x Unit))

    it "parses dot function calls with filled slots" $
      parseExpr "x.c( M::N::y)" `shouldBe` Right (Call c (Tuple x y))

    it "parses dot function calls with multiple slots" $
      parseExpr "x .d(M::N::y)  (f )" `shouldBe` Right (Call d (Tuple x (Tuple y f)))

    it "parses dot function calls with words and slots interleaved" $
      parseExpr "x. e (f) f(M ::N::G)" `shouldBe` Right (Call e (Tuple x (Tuple f g)))

    it "parses chained dot function calls" $
      parseExpr "x . a . M::N::B" `shouldBe` Right (Call b (Call a x))

    it "parses chained dot function calls with slots and nested calls" $
      parseExpr "x . c ( M::N::y.M::N::B ) . e (M::N::y) f()" `shouldBe`
        Right (Call e (Tuple (Call c (Tuple x (Call b y))) (Tuple y Unit)))

    it "parses calls of parenthesized expressions" $
      parseExpr "( f ) ( x )" `shouldBe` Right (Call f x)

    it "parses simple binary operator calls" $
      parseExpr "x + M::N::y" `shouldBe` Right (Call add (Tuple x y))

    it "parses mixed binary operator calls with right-grouping precedence" $
      parseExpr "1 + 2 * 3" `shouldBe` Right
        (Call add (Tuple (LitUInt 1) (Call mul (Tuple (LitUInt 2) (LitUInt 3)))))

    it "parses mixed binary operator calls with left-grouping precedence" $
      parseExpr "2 * 3 + 1" `shouldBe` Right
        (Call add (Tuple (Call mul (Tuple (LitUInt 2) (LitUInt 3))) (LitUInt 1)))

    it "parses left-associative binary operator calls" $
      parseExpr "1 + 2 + 3" `shouldBe` Right
        (Call add (Tuple (Call add (Tuple (LitUInt 1) (LitUInt 2))) (LitUInt 3)))

    it "parses right-associative binary operator calls" $
      parseExpr "1 || 2 || 3" `shouldBe` Right
        (Call logicOr (Tuple (LitUInt 1) (Call logicOr (Tuple (LitUInt 2) (LitUInt 3)))))

    it "rejects ambiguous binary operator calls" $
      parseExpr "f >> g << h" `shouldSatisfy` isLeft

    it "parses tuples" $
      parseExpr "1, 2, 3" `shouldBe` Right (Tuple (LitUInt 1) (Tuple (LitUInt 2) (LitUInt 3)))

    it "parses binary operator calls as function arguments" $
      parseExpr "f ( x + M::N::y )" `shouldBe` Right (Call f (Call add (Tuple x y)))

    it "parses tuples as function arguments" $
      parseExpr "f ( x , M::N::y )" `shouldBe` Right (Call f (Tuple x y))

    it "parses simple functions" $
      parseExpr "{ x }" `shouldBe` Right (Func PatIgnore x)

    it "parses function with arguments" $
      parseExpr "| x | { x }" `shouldBe` Right (Func xPat x)

    it "parses functions with assignments" $
      parseExpr "{ x = M::N::y ; x }" `shouldBe` Right (Func PatIgnore $ Let xPat y x)

    it "parses functions with evaluation statements" $
      parseExpr "{ f ( ) ; x }" `shouldBe` Right (Func PatIgnore $ Let PatIgnore (Call f Unit) x)

    it "parses functions which implicitly return unit" $
      parseExpr "{ f ( ) ; }" `shouldBe` Right (Func PatIgnore $ Let PatIgnore (Call f Unit) Unit)

    it "parses functions with multiple statements" $
      parseExpr "{ f ( ) ; x = M::N::y ; x }" `shouldBe`
        Right (Func PatIgnore $ Let PatIgnore (Call f Unit) $ Let xPat y x)

    it "parses simple 'do'-notation functions" $
      parseExpr "do x" `shouldBe` Right (Func PatIgnore x)

    it "parses 'do'-notation functions with arguments" $
      parseExpr "| x | do x" `shouldBe` Right (Func xPat x)

    it "parses 'do'-notation functions with assignments" $
      parseExpr "do x = M::N::y ; x" `shouldBe` Right (Func PatIgnore $ Let xPat y x)

    it "parses 'do'-notation functions with evaluation statements" $
      parseExpr "do f ( ) ; x" `shouldBe` Right (Func PatIgnore $ Let PatIgnore (Call f Unit) x)

    it "parses 'do'-notation functions which implicitly return unit" $
      parseExpr "do f ( ) ;" `shouldBe` Right (Func PatIgnore $ Let PatIgnore (Call f Unit) Unit)

    it "parses functions with multiple statements" $
      parseExpr "do f ( ) ; x = M::N::y ; x" `shouldBe`
        Right (Func PatIgnore $ Let PatIgnore (Call f Unit) $ Let xPat y x)

    it "parses 'do'-notation functions with no space between 'do' and the body" $
      parseExpr "do()" `shouldBe` Right (Func PatIgnore Unit)

    it "parses 'do'-notation functions not terminated by EOF" $
      parseExpr "( do f ( ) ; x = M::N::y ; x )" `shouldBe`
        Right (Func PatIgnore $ Let PatIgnore (Call f Unit) $ Let xPat y x)

    it "parses nested 'do'-notation functions" $
      parseExpr "f do x ; M::N::G do h ( ) ( ) ; M::N::y" `shouldBe`
        Right
          (Call
            f
            (Func
              PatIgnore
              $ Let PatIgnore x
              (Call g (Func PatIgnore $ Let PatIgnore (Call h (Tuple Unit Unit)) y))))

    -- If assignment statements are not parsed carefully, this test will not pass.
    it "parses functions with statements which are invocations of operators beginning with '='" $
      parseExpr "{ x == M::N::y ; }" `shouldBe`
        Right (Func PatIgnore $ Let PatIgnore (Call equ (Tuple x y)) Unit)

    it "parses empty functions" $
      parseExpr "{}" `shouldBe` Right (Func PatIgnore Unit)

    it "parses comments as insignificant whitespace" $
      parseExpr "f//comment\n(x)" `shouldBe` Right (Call f x)

    it "parses comments as significant whitespace" $
      parseExpr "k//comment\nl ( )" `shouldBe` Right (Call k Unit)

    it "parses comments mixed with spaces" $
      parseExpr "f // comment 1 \n \t \n // comment 2 \n \n ( x )" `shouldBe` Right (Call f x)

    it "parses comments terminated by EOF" $
      parseExpr "x//comment" `shouldBe` Right x
