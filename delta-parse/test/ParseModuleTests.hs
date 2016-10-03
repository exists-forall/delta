{-# LANGUAGE OverloadedStrings #-}

module ParseModuleTests (test) where

import Data.Text.Lazy (Text)

import Test.Hspec

import ParseModule
import Syntax
import StripMarks (stripModuleMarks)
import ParseUtils
import SyntaxUtils

parseModule :: Text -> Either ParseError Module
parseModule = fmap stripModuleMarks . fullParse module_

test :: Spec
test = describe "ParseModule" $ do
  it "parses minimal modules" $
    parseModule " export { } " `shouldBe` Right (Module (SymbolsSpecific []) [] [])

  it "parses modules which export everything" $
    parseModule " export { everything } " `shouldBe` Right (Module SymbolsEverything [] [])

  it "parses modules which export everything with a trailing semicolon" $
    parseModule " export { everything ; } " `shouldBe` Right (Module SymbolsEverything [] [])

  it "parses modules with def exports" $
    parseModule " export { x } " `shouldBe` Right
      (Module (SymbolsSpecific [SymbolDef $ NoAlias $ simpleVar X]) [] [])

  it "parses modules with def exports with multiple slots and words" $
    parseModule "export { f ( ) g ( ) }" `shouldBe` Right
      (Module
        (SymbolsSpecific
          [ SymbolDef
          $ NoAlias
          $ VarIdent (simpleIdent F)
          $ BodySlot
          $ TailWord (simpleIdent G)
          $ TailSlot
          $ EmptyTail
          ]
        )
        [] []
      )

  it "parses modules with dot-notation def exports" $
    parseModule "export { . f ( ) g ( ) }" `shouldBe` Right
      (Module
        (SymbolsSpecific
          [ SymbolDef
          $ NoAlias
          $ DotVarIdent (simpleIdent F)
          $ TailSlot
          $ TailWord (simpleIdent G)
          $ TailSlot
          $ EmptyTail
          ]
        )
        [] []
      )

  it "parses modules with binary-operator-notation def exports" $
    parseModule "export { == }" `shouldBe` Right
      (Module (SymbolsSpecific [SymbolDef $ NoAlias $ OperatorIdent OpEqu]) [] [])

  it "parses modules with prefix-operator-notation def exports" $
    parseModule "export { - ( ) }" `shouldBe` Right
      (Module (SymbolsSpecific [SymbolDef $ NoAlias $ PrefixOperatorIdent OpNegate]) [] [])

  it "parses modules with escaped def exports" $
    parseModule "export { ` do ` }" `shouldBe` Right
      (Module
        (SymbolsSpecific
          [ SymbolDef
          $ NoAlias
          $ VarIdent (Ident (Alpha LowerCase D) [StartChar $ Alpha LowerCase O])
          $ BodySlot
          $ EmptyTail
          ]
        )
        [] []
      )

  it "parses modules with aliased def exports" $
    parseModule "export { x = y }" `shouldBe` Right
      (Module (SymbolsSpecific [SymbolDef $ Alias (simpleVar X) (simpleVar Y)]) [] [])

  it "parses modules with type exports" $
    parseModule "export { type A }" `shouldBe` Right
      (Module (SymbolsSpecific [SymbolType $ NoAlias $ simpleTIdent A]) [] [])

  it "parses modules with escaped type exports" $
    parseModule "export { type ` Pure ` }" `shouldBe` Right
      (Module (SymbolsSpecific [SymbolType $ NoAlias $ typeIdentPure]) [] [])

  it "parses modules with aliased type exports" $
    parseModule "export { type A = B }" `shouldBe` Right
      (Module (SymbolsSpecific [SymbolType $ Alias (simpleTIdent A) (simpleTIdent B)]) [] [])

  it "parses modules with interaction exports" $
    parseModule "export { interaction A }" `shouldBe` Right
      (Module (SymbolsSpecific [SymbolInteraction $ NoAlias (simpleTIdent A)]) [] [])

  it "parses modules with escaped interaction exports" $
    parseModule "export { interaction ` Pure ` }" `shouldBe` Right
      (Module (SymbolsSpecific [SymbolInteraction $ NoAlias typeIdentPure]) [] [])

  it "parses modules with aliased interaction exports" $
    parseModule "export { interaction A = B }" `shouldBe` Right
      (Module (SymbolsSpecific [SymbolInteraction $ Alias (simpleTIdent A) (simpleTIdent B)]) [] [])

  it "parses modules with protocol exports" $
    parseModule "export { protocol A }" `shouldBe` Right
      (Module (SymbolsSpecific [SymbolProtocol $ NoAlias $ simpleTIdent A]) [] [])

  it "parses modules with escaped protocol exports" $
    parseModule "export { protocol ` Pure ` }" `shouldBe` Right
      (Module (SymbolsSpecific [SymbolProtocol $ NoAlias typeIdentPure]) [] [])

  it "parses modules with aliased protocol exports" $
    parseModule "export { protocol A = B }" `shouldBe` Right
      (Module (SymbolsSpecific [SymbolProtocol $ Alias (simpleTIdent A) (simpleTIdent B)]) [] [])

  it "parses modules with multiple exports" $
    parseModule "export { x ; y }" `shouldBe` Right
      (Module
        (SymbolsSpecific [SymbolDef $ NoAlias $ simpleVar X, SymbolDef $ NoAlias $ simpleVar Y])
        [] []
      )

  it "parses modules with multiple exports with a trailing semicolon" $
    parseModule "export { x ; y ; }" `shouldBe` Right
      (Module
        (SymbolsSpecific [SymbolDef $ NoAlias $ simpleVar X, SymbolDef $ NoAlias $ simpleVar Y])
        [] []
      )

  let none = SymbolsSpecific []

  it "parses modules with minimal imports" $
    parseModule "export { } import A ; " `shouldBe` Right
      (Module none [Import (NoAlias $ Path [] $ simpleModule A) none] [])

  it "parses modules with imports of modules with multiple path components" $
    parseModule "export { } import A :: B :: C ; " `shouldBe` Right
      (Module
        none
        [Import (NoAlias $ Path [simpleModule A, simpleModule B] $ simpleModule C) none]
        []
      )

  it "parses modules with aliased imports of modules" $
    parseModule "export { } import A = B ; " `shouldBe` Right
      (Module
        none
        [Import (Alias (Path [] $ simpleModule A) (Path [] $ simpleModule B)) none]
        []
      )

  -- Imported symbols don't need to be tested as thoroughly as exported symbols, because they're
  -- parsed using the same logic.
  it "parses modules which import specific symbols" $
    parseModule "export { } import A { x } " `shouldBe` Right
      (Module
        none
        [ Import
          (NoAlias $ Path [] $ simpleModule A)
          (SymbolsSpecific [SymbolDef $ NoAlias $ simpleVar X])
        ]
        []
      )

  it "parses modules with multiple imports" $
    parseModule "export { } import A ; import B ; " `shouldBe` Right
      (Module
        none
        [ Import (NoAlias $ Path [] $ simpleModule A) none
        , Import (NoAlias $ Path [] $ simpleModule B) none
        ]
        []
      )

  it "parses modules with declarations" $
    parseModule "export { } x : A = y ; " `shouldBe` Right
      (Module
        none
        []
        [DeclDef (PatVar (simpleVar X) (simpleType A)) [] (simpleVarExpr Y)]
      )

  it "parses modules with multiple declarations" $
    parseModule "export { } w : A = x ; y : B = z ; " `shouldBe` Right
      (Module
        none
        []
        [ DeclDef (PatVar (simpleVar W) (simpleType A)) [] (simpleVarExpr X)
        , DeclDef (PatVar (simpleVar Y) (simpleType B)) [] (simpleVarExpr Z)
        ]
      )
