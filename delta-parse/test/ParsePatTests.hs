{-# LANGUAGE OverloadedStrings #-}

module ParsePatTests (test) where

import Data.Text.Lazy (Text)

import Test.Hspec

import ParsePat

import Syntax
import ParseUtils

parsePat :: Text -> Either ParseError Pat
parsePat = fmap stripPatMarks . fullParse pat

test :: Spec
test = describe "ParsePat" $ do

  let x = PatVar $ VarIdent (Ident (Alpha LowerCase X) []) $ BodySlot EmptyTail
  let y = PatVar $ VarIdent (Ident (Alpha LowerCase Y) []) $ BodySlot EmptyTail
  let z = PatVar $ VarIdent (Ident (Alpha LowerCase Z) []) $ BodySlot EmptyTail

  let
    f = PatVar
      $ VarIdent (Ident (Alpha LowerCase F) [])
      $ BodySlot
      $ TailWord (Ident (Alpha LowerCase G) [])
      $ TailWord (Ident (Alpha LowerCase H) [])
      $ TailSlot
      $ EmptyTail


  describe "pat" $ do
    it "parses simple variable patterns" $
      parsePat "x" `shouldBe` Right x

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
