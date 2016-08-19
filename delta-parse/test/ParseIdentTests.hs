{-# LANGUAGE OverloadedStrings #-}

module ParseIdentTests (test) where

import Data.Either (isLeft)

import Data.Text.Lazy (Text)

import ParseUtils
import ParseIdent
import Syntax

import Test.Hspec

-- Trivial aliases for easy construction
l :: Letter -> IdentStartChar
l = Alpha LowerCase

u :: Letter -> IdentStartChar
u = Alpha UpperCase

l' :: Letter -> IdentChar
l' = StartChar . l

u' :: Letter -> IdentChar
u' = StartChar . u

i :: IdentStartChar -> [IdentChar] -> Ident
i = Ident

test :: Spec
test = describe "ParseIdent" $ do
  let fooIdent = i (l F) [l' O, l' O]
  let upperBarIdent = i (u B) [u' A, u' R]
  let bazIdent = i (l B) [l' A, l' Z]
  let bizIdent = i (l B) [l' I, l' Z]

  describe "ident" $ do
    it "parses lowercase identifiers" $
      fullParse ident "foo" `shouldBe` Right fooIdent

    it "parses uppercase identifiers" $
      fullParse ident "BAR" `shouldBe` Right upperBarIdent

    it "parses identifiers containing underscores" $
      fullParse ident "foo_bar" `shouldBe` Right (i (l F) [l' O, l' O, StartChar Underscore, l' B, l' A, l' R])

    it "parses identifiers starting with underscores" $
      fullParse ident "_foo" `shouldBe` Right (i Underscore [l' F, l' O, l' O])

    it "parses identifiers containing digits" $
      fullParse ident "foo42" `shouldBe` Right (i (l F) [l' O, l' O, Digit D4, Digit D2])

    it "rejects identifiers starting with a digit" $
      fullParse ident "42foo" `shouldSatisfy` isLeft

    it "rejects identifiers containing non-ascii characters" $
      fullParse ident "pi_π" `shouldSatisfy` isLeft

  describe "moduleIdent" $ do
    it "parses module identifiers" $
      fullParse moduleIdent "Foo_42" `shouldBe`
        Right (ModuleIdent F [l' O, l' O, StartChar Underscore, Digit D4, Digit D2])

    it "rejects module identifiers starting with a lowercase letter" $
      fullParse moduleIdent "foo_42" `shouldSatisfy` isLeft

    it "rejects module identifiers starting with an underscore" $
      fullParse moduleIdent "_foo_42" `shouldSatisfy` isLeft

    it "rejects module identifiers starting with a digit" $
      fullParse moduleIdent "42Foo" `shouldSatisfy` isLeft

  describe "varIdent" $ do
    it "parses single-identifier variables" $
      fullParse varIdent "foo" `shouldBe`
        Right (VarIdent fooIdent $ BodySlot $ EmptyTail)

    it "parses single-identifier variables with an explicit slot" $
      fullParse varIdent "foo()" `shouldBe`
        Right (VarIdent fooIdent $ BodySlot EmptyTail)

    it "parses variables with multiple words" $
      fullParse varIdent "foo BAR baz ()" `shouldBe`
        Right (VarIdent fooIdent $ BodyWord upperBarIdent $ BodyWord bazIdent $ BodySlot EmptyTail)

    it "parses variables with multiple slots" $
      fullParse varIdent "foo () ()" `shouldBe`
        Right (VarIdent fooIdent $ BodySlot $ TailSlot EmptyTail)

    it "parses variables with words and slots interleaved" $
      fullParse varIdent "foo BAR () (  ) baz () biz" `shouldBe`
        Right (
          VarIdent fooIdent $
          BodyWord upperBarIdent $
          BodySlot $
          TailSlot $
          TailWord bazIdent $
          TailSlot $
          TailWord bizIdent $
          EmptyTail)

    it "rejects empty variables" $
      fullParse varIdent "" `shouldSatisfy` isLeft

    it "rejects variables without a single slot" $
      fullParse varIdent "foo bar" `shouldSatisfy` isLeft

    it "rejects variables beginning with a slot" $
      fullParse varIdent "() foo" `shouldSatisfy` isLeft

    it "parses dot variables" $
      fullParse varIdent ".foo" `shouldBe`
        Right (DotVarIdent fooIdent EmptyTail)

    it "parses dot variables with multiple words and whitespace" $
      fullParse varIdent ". foo BAR" `shouldBe`
        Right (DotVarIdent fooIdent $ TailWord upperBarIdent EmptyTail)

    it "parses dot variables with slots" $
      fullParse varIdent ".foo()" `shouldBe`
        Right (DotVarIdent fooIdent $ TailSlot EmptyTail)

    it "parses dot variables with multiple slots" $
      fullParse varIdent ".foo () ()" `shouldBe`
        Right (DotVarIdent fooIdent $ TailSlot $ TailSlot EmptyTail)

    it "parses dot varaibles with words and slots interleaved" $
      fullParse varIdent ".foo BAR () ( ) baz () biz" `shouldBe`
        Right (
          DotVarIdent fooIdent $
          TailWord upperBarIdent $
          TailSlot $
          TailSlot $
          TailWord bazIdent $
          TailSlot $
          TailWord bizIdent $
          EmptyTail)

    it "rejects empty dot variables" $
      fullParse varIdent "." `shouldSatisfy` isLeft

    it "rejects dot variables beginning with a slot" $
      fullParse varIdent ".() foo" `shouldSatisfy` isLeft

  describe "typeIdent" $ do
    it "parses type identifiers" $
      fullParse typeIdent "Foo_42" `shouldBe`
        Right (TypeIdent F [l' O, l' O, StartChar Underscore, Digit D4, Digit D2])

    it "rejects type identifiers starting with a lowercase letter" $
      fullParse typeIdent "foo_42" `shouldSatisfy` isLeft

    it "rejects type identifiers starting with an underscore" $
      fullParse typeIdent "_foo_42" `shouldSatisfy` isLeft

    it "rejects type identifiers starting with a digit" $
      fullParse typeIdent "42Foo" `shouldSatisfy` isLeft
