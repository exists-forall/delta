{-# LANGUAGE OverloadedStrings #-}

module ParsePatTests (test) where

import Data.Text.Lazy (Text)

import Test.Hspec

import ParsePat

import Syntax
import ParseUtils

parsePat :: Text -> Either ParseError Pat
parsePat = fmap stripPatMarks . fullParse pat

simpleIdent :: Letter -> VarIdent
simpleIdent l = VarIdent (Ident (Alpha LowerCase l) []) $ BodySlot EmptyTail

simpleType :: Letter -> Type
simpleType l = TypeAtom $ Path [] $ TypeIdent l []

test :: Spec
test = describe "ParsePat" $ do

  let x = PatVar (simpleIdent X) Nothing
  let xAsA = PatVar (simpleIdent X) (Just $ simpleType A)

  let y = PatVar (simpleIdent Y) Nothing
  let yAsB = PatVar (simpleIdent Y) (Just $ simpleType B)

  let z = PatVar (simpleIdent Z) Nothing
  let zAsTupleAB = PatVar (simpleIdent Z) (Just $ TypeTuple (simpleType A) (simpleType B))

  let wAsFuncAB = PatVar (simpleIdent W) (Just $ TypeFunc (simpleType A) TypePure (simpleType B))

  let
    f = flip PatVar Nothing
      $ VarIdent (Ident (Alpha LowerCase F) [])
      $ BodySlot
      $ TailWord (Ident (Alpha LowerCase G) [])
      $ TailWord (Ident (Alpha LowerCase H) [])
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
      parsePat "_" `shouldBe` Right PatIgnore

    it "parses unit patterns" $
      parsePat "( )" `shouldBe` Right PatUnit

    it "parses tuple patterns" $
      parsePat "x , y , z" `shouldBe` Right (PatTuple x (PatTuple y z))

    it "parses nested patterns" $
      parsePat "( x , y ) , ( _ ) , ( ( ) , ( ) ) , z" `shouldBe` Right
        (PatTuple (PatTuple x y) $ PatTuple PatIgnore $ PatTuple (PatTuple PatUnit PatUnit) $ z)

    it "parses escaped variables in patterns" $
      parsePat "` f ( ) g h ( ) `" `shouldBe` Right f
