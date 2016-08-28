{-# LANGUAGE OverloadedStrings #-}

module ParseTypeTests (test) where

import Test.Hspec

import ParseType
import Syntax
import ParseUtils

import Data.Either (isLeft)

test :: Spec
test = describe "ParseType" $ do
  let
    a = TypeAtom $ Path [] $ TypeIdent A []
    b = TypeAtom $ Path [] $ TypeIdent B []
    c = TypeAtom $ Path [ModuleIdent M [], ModuleIdent N []] $ TypeIdent C []

    pureAtom = TypeAtom $ Path [] $ TypeIdent P $ map (StartChar . Alpha LowerCase) [U, R, E]

    x = TypeVar $ TypeVarIdent X []
    y = TypeVar $ TypeVarIdent Y []

    doVar = TypeVar $ TypeVarIdent D [StartChar (Alpha LowerCase O)]

  describe "type_" $ do
    it "parses unit" $
      fullParse type_ "( )" `shouldBe` Right TypeUnit

    it "parses `Pure`" $
      fullParse type_ "Pure" `shouldBe` Right TypePure

    it "parses `Never`" $
      fullParse type_ "Never" `shouldBe` Right TypeNever

    it "parses simple atom types" $
      fullParse type_ "A" `shouldBe` Right a

    it "parses qualified atom types" $
      fullParse type_ "M :: N :: C" `shouldBe` Right c

    it "parses escaped atom types" $
      fullParse type_ "`Pure`" `shouldBe` Right pureAtom

    it "parses type variables" $
      fullParse type_ "x" `shouldBe` Right x

    it "rejects qualified type variables" $
      fullParse type_ "M::x" `shouldSatisfy` isLeft

    it "rejects type variables which are reserved words" $
      fullParse type_ "do" `shouldSatisfy` isLeft

    it "parses escaped type variables" $
      fullParse type_ "`do`" `shouldBe` Right doVar

    it "parses simple type applications" $
      fullParse type_ "A < B >" `shouldBe` Right (TypeApp a b)

    it "parses multiple left-associative type applications" $
      fullParse type_ "A < B > < x >" `shouldBe` Right (TypeApp (TypeApp a b) x)

    it "parses type applications where the head is a type variable" $
      fullParse type_ "x < y >" `shouldBe` Right (TypeApp x y)

    it "parses type applications where the head is parenthesized" $
      fullParse type_ "( A < B > ) < x >" `shouldBe` Right (TypeApp (TypeApp a b) x)

    it "parses nested type applications" $
      fullParse type_ "A < B < M :: N :: C > >" `shouldBe` Right (TypeApp a (TypeApp b c))

    it "parses interactions" $
      fullParse type_ "A < B > | x < y > | M :: N :: C" `shouldBe`
        Right (TypeInters (TypeInters (TypeApp a b) (TypeApp x y)) c)

    it "parses pure functions" $
      fullParse type_ "A -> B" `shouldBe` Right (TypeFunc a TypePure b)

    it "parses impure functions" $
      fullParse type_ "A ! x -> B" `shouldBe` Right (TypeFunc a x b)

    it "parses functions with compound argument and return types" $
      fullParse type_ "A < B > -> x < y >" `shouldBe`
        Right (TypeFunc (TypeApp a b) TypePure (TypeApp x y))

    it "parses functions with multiple interactions" $
      fullParse type_ "A ! x | y -> B" `shouldBe` Right (TypeFunc a (TypeInters x y) b)

    it "parses curried functions" $
      fullParse type_ "A -> B -> M :: N :: C" `shouldBe`
        Right (TypeFunc a TypePure (TypeFunc b TypePure c))

    it "parses functions grouped by parentheses" $
      fullParse type_ "( A -> B ) -> M :: N :: C" `shouldBe`
        Right (TypeFunc (TypeFunc a TypePure b) TypePure c)

    it "parses tuples" $
      fullParse type_ "A , B , M :: N :: C" `shouldBe` Right (TypeTuple a $ TypeTuple b c)

    it "parses functions with higher precedence than tuples" $
      fullParse type_ "x , A -> B , y" `shouldBe`
        Right (TypeTuple x $ TypeTuple (TypeFunc a TypePure b) y)

    it "parses tuples grouped by parnetheses" $
      fullParse type_ "( A , B ) , M :: N :: C" `shouldBe` Right (TypeTuple (TypeTuple a b) c)
