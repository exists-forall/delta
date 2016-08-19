{-# LANGUAGE OverloadedStrings #-}

module ParseExprTests (test) where

import Test.Hspec

import ParseExpr
import Syntax
import ParseUtils

simpleIdent :: Letter -> Ident
simpleIdent l = Ident (Alpha LowerCase l) []

simpleUpperIdent :: Letter -> Ident
simpleUpperIdent l = Ident (Alpha UpperCase l) []

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
      fullParse expr "x" `shouldBe` Right x

    it "parses qualified identifiers" $
      fullParse expr "M::N :: y" `shouldBe` Right y

    it "parses nullary function calls" $
      fullParse expr "f()" `shouldBe` Right (Call f Unit)

    it "parses unary function calls" $
      fullParse expr "f(x)" `shouldBe` Right (Call f x)

    it "parses qualified function calls" $
      fullParse expr "M :: N::G()" `shouldBe` Right (Call g Unit)

    it "parses nested function calls" $
      fullParse expr "f(  M::  N    :: G ( x ))" `shouldBe` Right (Call f (Call g x))

    it "parses function calls with multiple slots" $
      fullParse expr "h()   (  )" `shouldBe` Right (Call h (Tuple Unit Unit))

    it "parses function calls with multiple filled slots" $
      fullParse expr "h (x )( M::N::y)" `shouldBe` Right (Call h (Tuple x y))

    it "parses function calls with words and slots interleaved" $
      fullParse expr "i ()j (  )" `shouldBe` Right (Call i (Tuple Unit Unit))

    it "parses function calls with words and filled slots interleaved" $
      fullParse expr "i (  x) j(M::N :: y)" `shouldBe` Right (Call i (Tuple x y))

    it "parses dot function calls" $
      fullParse expr "x.a" `shouldBe` Right (Call a x)

    it "parses qualified dot function calls" $
      fullParse expr "x . M ::N::  B" `shouldBe` Right (Call b x)

    it "parses dot function calls with slots" $
      fullParse expr "x.c ( )" `shouldBe` Right (Call c (Tuple x Unit))

    it "parses dot function calls with filled slots" $
      fullParse expr "x.c( M::N::y)" `shouldBe` Right (Call c (Tuple x y))

    it "parses dot function calls with multiple slots" $
      fullParse expr "x .d(M::N::y)  (f )" `shouldBe` Right (Call d (Tuple x (Tuple y f)))

    it "parses dot function calls with words and slots interleaved" $
      fullParse expr "x. e (f) f(M ::N::G)" `shouldBe` Right (Call e (Tuple x (Tuple f g)))

    it "parses chained dot function calls" $
      fullParse expr "x . a . M::N::B" `shouldBe` Right (Call b (Call a x))

    it "parses chained dot function calls with slots and nested calls" $
      fullParse expr "x . c ( M::N::y.M::N::B ) . e (M::N::y) f()" `shouldBe`
        Right (Call e (Tuple (Call c (Tuple x (Call b y))) (Tuple y Unit)))
