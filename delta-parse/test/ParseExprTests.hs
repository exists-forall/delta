{-# LANGUAGE OverloadedStrings #-}

module ParseExprTests (test) where

import Data.Text.Lazy (Text)

import Test.Hspec

import ParseExpr
import Syntax
import ParseUtils

simpleIdent :: Letter -> Ident
simpleIdent l = Ident (Alpha LowerCase l) []

simpleUpperIdent :: Letter -> Ident
simpleUpperIdent l = Ident (Alpha UpperCase l) []

parseExpr :: Text -> Either ParseError Expr
parseExpr = fmap stripMarks . fullParse expr

test :: Spec
test = describe "ParseExpr" $ do
  let m = ModuleIdent M []
  let n = ModuleIdent N []

  let x = Var (Path [] (VarIdent (simpleIdent X) $ BodySlot EmptyTail))
  let y = Var (Path [m, n] (VarIdent (simpleIdent Y) $ BodySlot EmptyTail))

  let f = Var (Path [] (VarIdent (simpleIdent F) $ BodySlot EmptyTail))
  let g = Var (Path [m, n] (VarIdent (simpleUpperIdent G) $ BodySlot EmptyTail))
  let h = Var (Path [] (VarIdent (simpleIdent H) $ BodySlot $ TailSlot EmptyTail))
  let i = Var (Path [] (VarIdent (simpleIdent I) $ BodySlot $ TailWord (simpleIdent J) $ TailSlot EmptyTail))

  let a = Var (Path [] (DotVarIdent (simpleIdent A) EmptyTail))
  let b = Var (Path [m, n] (DotVarIdent (simpleUpperIdent B) EmptyTail))
  let c = Var (Path [] (DotVarIdent (simpleIdent C) $ TailSlot EmptyTail))
  let d = Var (Path [] (DotVarIdent (simpleIdent D) $ TailSlot $ TailSlot EmptyTail))
  let e = Var (Path [] (DotVarIdent (simpleIdent E) $ TailSlot $ TailWord (simpleIdent F) $ TailSlot EmptyTail))

  describe "expr" $ do
    it "parses simple identifiers" $
      parseExpr "x" `shouldBe` Right x

    it "parses qualified identifiers" $
      parseExpr "M::N :: y" `shouldBe` Right y

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
