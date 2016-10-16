{-# LANGUAGE OverloadedStrings #-}

module ParseTypeTests (test) where

import Data.Text.Lazy (Text)

import Test.Hspec

import ParseType
import Delta.Structures.Syntax
import StripMarks (stripTypeMarks)
import ParseUtils
import SyntaxUtils

import Data.Either (isLeft)

parseType :: Text -> Either ParseError Type
parseType = fmap stripTypeMarks . fullParse type_

test :: Spec
test = describe "ParseType" $ do
  let
    a = simpleType A
    b = simpleType B
    c = TypeAtom $ Path [simpleModule M, simpleModule N] $ simpleTIdent C

    pureAtom = TypeAtom $ Path [] $ typeIdentText $ TypeIdent P $ map (StartChar . Alpha LowerCase) [U, R, E]

    x = TypeVar $ simpleTypeVar X
    y = TypeVar $ simpleTypeVar Y

    doVar = TypeVar $ typeVarIdentText $ TypeVarIdent D [StartChar (Alpha LowerCase O)]

  describe "type_" $ do
    it "parses unit" $
      parseType "( )" `shouldBe` Right TypeUnit

    it "parses `Pure`" $
      parseType "Pure" `shouldBe` Right TypePure

    it "parses `Never`" $
      parseType "Never" `shouldBe` Right TypeNever

    it "parses simple atom types" $
      parseType "A" `shouldBe` Right a

    it "parses qualified atom types" $
      parseType "M :: N :: C" `shouldBe` Right c

    it "parses escaped atom types" $
      parseType "`Pure`" `shouldBe` Right pureAtom

    it "parses type variables" $
      parseType "x" `shouldBe` Right x

    it "rejects qualified type variables" $
      parseType "M::x" `shouldSatisfy` isLeft

    it "rejects type variables which are reserved words" $
      parseType "do" `shouldSatisfy` isLeft

    it "parses escaped type variables" $
      parseType "`do`" `shouldBe` Right doVar

    it "parses simple type applications" $
      parseType "A < B >" `shouldBe` Right (TypeApp a b)

    it "parses multiple left-associative type applications" $
      parseType "A < B > < x >" `shouldBe` Right (TypeApp (TypeApp a b) x)

    it "parses type applications where the head is a type variable" $
      parseType "x < y >" `shouldBe` Right (TypeApp x y)

    it "parses type applications where the head is parenthesized" $
      parseType "( A < B > ) < x >" `shouldBe` Right (TypeApp (TypeApp a b) x)

    it "parses nested type applications" $
      parseType "A < B < M :: N :: C > >" `shouldBe` Right (TypeApp a (TypeApp b c))

    it "parses interactions" $
      parseType "A < B > | x < y > | M :: N :: C" `shouldBe`
        Right (TypeInters (TypeInters (TypeApp a b) (TypeApp x y)) c)

    it "parses pure functions" $
      parseType "A -> B" `shouldBe` Right (TypeFunc a TypePure b)

    it "parses impure functions" $
      parseType "A ! x -> B" `shouldBe` Right (TypeFunc a x b)

    it "parses functions with compound argument and return types" $
      parseType "A < B > -> x < y >" `shouldBe`
        Right (TypeFunc (TypeApp a b) TypePure (TypeApp x y))

    it "parses functions with multiple interactions" $
      parseType "A ! x | y -> B" `shouldBe` Right (TypeFunc a (TypeInters x y) b)

    it "parses curried functions" $
      parseType "A -> B -> M :: N :: C" `shouldBe`
        Right (TypeFunc a TypePure (TypeFunc b TypePure c))

    it "parses functions grouped by parentheses" $
      parseType "( A -> B ) -> M :: N :: C" `shouldBe`
        Right (TypeFunc (TypeFunc a TypePure b) TypePure c)

    it "parses tuples" $
      parseType "A , B , M :: N :: C" `shouldBe` Right (TypeTuple a $ TypeTuple b c)

    it "parses functions with higher precedence than tuples" $
      parseType "x , A -> B , y" `shouldBe`
        Right (TypeTuple x $ TypeTuple (TypeFunc a TypePure b) y)

    it "parses tuples grouped by parnetheses" $
      parseType "( A , B ) , M :: N :: C" `shouldBe` Right (TypeTuple (TypeTuple a b) c)
