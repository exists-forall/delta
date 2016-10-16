{-# LANGUAGE OverloadedStrings #-}

module ParsePatTests (test) where

import Data.Text.Lazy (Text)

import Test.Hspec

import ParsePat

import Syntax
import StripMarks (stripPatMarks, stripTypeMarks)
import ParseUtils
import SyntaxUtils

import Data.Either (isLeft)

parsePat :: Text -> Either ParseError Pat
parsePat = fmap (stripPatMarks (fmap stripTypeMarks)) . fullParse pat

parseTypedPat :: Text -> Either ParseError TypedPat
parseTypedPat = fmap (stripPatMarks stripTypeMarks) . fullParse typedPat

test :: Spec
test = describe "ParsePat" $ do

  let x = PatVar (simpleVar X) Nothing
  let xAsA = PatVar (simpleVar X) (Just $ simpleType A)
  let xAsARequired = PatVar (simpleVar X) (simpleType A)

  let y = PatVar (simpleVar Y) Nothing
  let yAsB = PatVar (simpleVar Y) (Just $ simpleType B)
  let yAsBRequired = PatVar (simpleVar Y) (simpleType B)

  let z = PatVar (simpleVar Z) Nothing
  let zAsTupleAB = PatVar (simpleVar Z) (Just $ TypeTuple (simpleType A) (simpleType B))
  let zAsTupleABRequired = PatVar (simpleVar Z) (TypeTuple (simpleType A) (simpleType B))

  let wAsFuncAB = PatVar (simpleVar W) (Just $ TypeFunc (simpleType A) TypePure (simpleType B))
  let wAsFuncABRequired = PatVar (simpleVar W) (TypeFunc (simpleType A) TypePure (simpleType B))

  let ignore = PatIgnore Nothing
  let ignoreAsA = PatIgnore (Just $ simpleType A)
  let ignoreAsARequired = PatIgnore (simpleType A)

  let
    f = flip PatVar Nothing
      $ varIdentText
      $ VarIdent (simpleIdent F)
      $ BodySlot
      $ TailWord (simpleIdent G)
      $ TailWord (simpleIdent H)
      $ TailSlot
      $ EmptyTail

  describe "pat" $ do
    it "parses simple variable patterns" $
      parsePat "x" `shouldBe` Right x

    it "parses typed variable patterns" $
      parsePat "x : A" `shouldBe` Right xAsA

    it "parses tuples of typed variable patterns" $
      parsePat "x : A , y : B" `shouldBe` Right (PatTuple xAsA yAsB)

    it "parses mixed typed and untyped variable patterns" $
      parsePat "x : A , y" `shouldBe` Right (PatTuple xAsA y)

    it "parses variable patterns typed by parenthesized tuples" $
      parsePat "z : ( A , B )" `shouldBe` Right zAsTupleAB

    it "parses variable patterns typed by unparenthesized functions" $
      parsePat "w : A -> B , x" `shouldBe` Right (PatTuple wAsFuncAB x)

    it "parses ignore patterns" $
      parsePat "_" `shouldBe` Right ignore

    it "parses typed ignore patterns" $
      parsePat "_ : A" `shouldBe` Right ignoreAsA

    it "parses unit patterns" $
      parsePat "( )" `shouldBe` Right PatUnit

    it "parses tuple patterns" $
      parsePat "x , y , z" `shouldBe` Right (PatTuple x (PatTuple y z))

    it "parses nested patterns" $
      parsePat "( x , y ) , ( _ ) , ( ( ) , ( ) ) , z" `shouldBe` Right
        (PatTuple (PatTuple x y) $ PatTuple ignore $ PatTuple (PatTuple PatUnit PatUnit) $ z)

    it "parses escaped variables in patterns" $
      parsePat "` f ( ) g h ( ) `" `shouldBe` Right f

  describe "typedPat" $ do
    it "rejects untyped variable patterns" $
      parseTypedPat "x" `shouldSatisfy` isLeft

    it "parses typed variable patterns" $
      parseTypedPat "x : A" `shouldBe` Right xAsARequired

    it "parses tuples of typed variable patterns" $
      parseTypedPat "x : A , y : B" `shouldBe` Right (PatTuple xAsARequired yAsBRequired)

    it "rejects mixed typed and untyped variable patterns" $
      parseTypedPat "x : A , y" `shouldSatisfy` isLeft

    it "parses variable patterns typed by parenthesized tuples" $
      parseTypedPat "z : ( A , B )" `shouldBe` Right zAsTupleABRequired

    it "parses variable patterns typed by unparenthesized functions" $
      parseTypedPat "w : A -> B , x : A" `shouldBe` Right (PatTuple wAsFuncABRequired xAsARequired)

    it "rejects untyped ignore patterns" $
      parseTypedPat "_" `shouldSatisfy` isLeft

    it "parses typed ignore patterns" $
      parseTypedPat "_ : A" `shouldBe` Right ignoreAsARequired

    it "parses unit patterns" $
      parseTypedPat "( )" `shouldBe` Right PatUnit

    it "parses nested patterns" $
      parseTypedPat "( x : A , y : B ) , ( _ : A ) , ( ( ) , ( ) ) , z : ( A , B )" `shouldBe` Right
        ( PatTuple (PatTuple xAsARequired yAsBRequired)
        $ PatTuple ignoreAsARequired
        $ PatTuple (PatTuple PatUnit PatUnit)
        $ zAsTupleABRequired
        )
