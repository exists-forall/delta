{-# LANGUAGE OverloadedStrings #-}

module ParseIdentTests (test) where

import Data.Either (isLeft)

import ParseUtils
import ParseIdent
import Delta.Structures.Syntax hiding (path)

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

  {-
  For extra conservatism in catching bugs, tests which expect successful parsing ("parses...")
  restrict the space of accepted strings using `ForbidReserved`, and tests which expect parse
  failure ("rejects...") widen the space of accepted strings using `AllowReserved`, except where
  the test specifically involves special behavior regarding reserved words.
  -}

  describe "ident" $ do
    it "parses lowercase identifiers" $
      fullParse ident "foo" `shouldBe` Right (identText fooIdent)

    it "parses uppercase identifiers" $
      fullParse ident "BAR" `shouldBe` Right (identText upperBarIdent)

    it "parses identifiers containing underscores" $
      fullParse ident "foo_bar" `shouldBe` Right (identText (i (l F) [l' O, l' O, StartChar Underscore, l' B, l' A, l' R]))

    it "parses identifiers starting with underscores" $
      fullParse ident "_foo" `shouldBe` Right (identText (i Underscore [l' F, l' O, l' O]))

    it "parses identifiers containing digits" $
      fullParse ident "foo42" `shouldBe` Right (identText (i (l F) [l' O, l' O, Digit D4, Digit D2]))

    it "rejects identifiers starting with a digit" $
      fullParse ident "42foo" `shouldSatisfy` isLeft

    it "rejects identifiers containing non-ascii characters" $
      fullParse ident "pi_π" `shouldSatisfy` isLeft

    it "rejects identifiers which are reserved words" $
      fullParse ident "do" `shouldSatisfy` isLeft

  describe "moduleIdent'" $ do
    it "parses module identifiers" $
      fullParse (moduleIdent' ForbidReserved) "Foo_42" `shouldBe`
        Right (moduleIdentText (ModuleIdent F [l' O, l' O, StartChar Underscore, Digit D4, Digit D2]))

    it "rejects module identifiers starting with a lowercase letter" $
      fullParse (moduleIdent' AllowReserved) "foo_42" `shouldSatisfy` isLeft

    it "rejects module identifiers starting with an underscore" $
      fullParse (moduleIdent' AllowReserved) "_foo_42" `shouldSatisfy` isLeft

    it "rejects module identifiers starting with a digit" $
      fullParse (moduleIdent' AllowReserved) "42Foo" `shouldSatisfy` isLeft

  describe "varIdent'" $ do
    it "parses single-identifier variables" $
      fullParse (varIdent' ForbidReserved) "foo" `shouldBe`
        Right (VarIdent (identText fooIdent) $ BodySlot $ EmptyTail)

    it "parses single-identifier variables with an explicit slot" $
      fullParse (varIdent' ForbidReserved) "foo()" `shouldBe`
        Right (VarIdent (identText fooIdent) $ BodySlot EmptyTail)

    it "parses variables with multiple words" $
      fullParse (varIdent' ForbidReserved) "foo BAR baz ()" `shouldBe`
        Right (
          VarIdent (identText fooIdent) $
          BodyWord (identText upperBarIdent) $
          BodyWord (identText bazIdent) $
          BodySlot EmptyTail)

    it "parses variables with multiple slots" $
      fullParse (varIdent' ForbidReserved) "foo () ()" `shouldBe`
        Right (VarIdent (identText fooIdent) $ BodySlot $ TailSlot EmptyTail)

    it "parses variables with words and slots interleaved" $
      fullParse (varIdent' ForbidReserved) "foo BAR () (  ) baz () biz" `shouldBe`
        Right (
          VarIdent (identText fooIdent) $
          BodyWord (identText upperBarIdent) $
          BodySlot $
          TailSlot $
          TailWord (identText bazIdent) $
          TailSlot $
          TailWord (identText bizIdent) $
          EmptyTail)

    it "rejects empty variables" $
      fullParse (varIdent' AllowReserved) "" `shouldSatisfy` isLeft

    it "rejects variables without a single slot" $
      fullParse (varIdent' AllowReserved) "foo bar" `shouldSatisfy` isLeft

    it "rejects variables beginning with a slot" $
      fullParse (varIdent' AllowReserved) "() foo" `shouldSatisfy` isLeft

    it "parses dot variables" $
      fullParse (varIdent' ForbidReserved) ".foo" `shouldBe`
        Right (DotVarIdent (identText fooIdent) EmptyTail)

    it "parses dot variables with multiple words and whitespace" $
      fullParse (varIdent' ForbidReserved) ". foo BAR" `shouldBe`
        Right (DotVarIdent (identText fooIdent) $ TailWord (identText upperBarIdent) EmptyTail)

    it "parses dot variables with slots" $
      fullParse (varIdent' ForbidReserved) ".foo()" `shouldBe`
        Right (DotVarIdent (identText fooIdent) $ TailSlot EmptyTail)

    it "parses dot variables with multiple slots" $
      fullParse (varIdent' ForbidReserved) ".foo () ()" `shouldBe`
        Right (DotVarIdent (identText fooIdent) $ TailSlot $ TailSlot EmptyTail)

    it "parses dot varaibles with words and slots interleaved" $
      fullParse (varIdent' ForbidReserved) ".foo BAR () ( ) baz () biz" `shouldBe`
        Right (
          DotVarIdent (identText fooIdent) $
          TailWord (identText upperBarIdent) $
          TailSlot $
          TailSlot $
          TailWord (identText bazIdent) $
          TailSlot $
          TailWord (identText bizIdent) $
          EmptyTail)

    it "rejects empty dot variables" $
      fullParse (varIdent' AllowReserved) "." `shouldSatisfy` isLeft

    it "rejects dot variables beginning with a slot" $
      fullParse (varIdent' AllowReserved) ".() foo" `shouldSatisfy` isLeft

  describe "typeIdent'" $ do
    it "parses type identifiers" $
      fullParse (typeIdent' ForbidReserved) "Foo_42" `shouldBe`
        Right (typeIdentText $ TypeIdent F [l' O, l' O, StartChar Underscore, Digit D4, Digit D2])

    it "rejects type identifiers starting with a lowercase letter" $
      fullParse (typeIdent' AllowReserved) "foo_42" `shouldSatisfy` isLeft

    it "rejects type identifiers starting with an underscore" $
      fullParse (typeIdent' AllowReserved) "_foo_42" `shouldSatisfy` isLeft

    it "rejects type identifiers starting with a digit" $
      fullParse (typeIdent' AllowReserved) "42Foo" `shouldSatisfy` isLeft

  describe "path" $ do
    it "parses simple paths" $
      fullParse path "M::" `shouldBe` Right [moduleIdentText $ ModuleIdent M []]

    it "parses paths with multiple components" $
      fullParse path "M::N::" `shouldBe` Right [moduleIdentText $ ModuleIdent M [], moduleIdentText $ ModuleIdent N []]

    it "parses paths with whitespace between components" $
      fullParse path "M :: N ::" `shouldBe` Right [moduleIdentText $ ModuleIdent M [], moduleIdentText $ ModuleIdent N []]

    it "rejects paths containing reserved words" $
      fullParse path "M::Pure::" `shouldSatisfy` isLeft

    it "parses paths containing escaped reserved words" $
      fullParse path "M :: ` Pure ` ::" `shouldBe`
        Right [moduleIdentText $ ModuleIdent M [], moduleIdentText $ ModuleIdent P $ map (StartChar . Alpha LowerCase) [U, R, E]]
