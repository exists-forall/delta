module ParseModule
  (module_
  )
where

import ParseUtils

import qualified Syntax as Stx
import ParseIdent
import ParseDecl

assemblePossibleAlias :: a -> Maybe a -> Stx.PossibleAlias a
assemblePossibleAlias alias (Just original) = Stx.Alias alias original
assemblePossibleAlias original Nothing = Stx.NoAlias original

possibleAlias :: Parser a -> Parser (Stx.PossibleAlias a)
possibleAlias p =
  assemblePossibleAlias <$> (p <* spaces) <*> optionMaybe (char '=' *> spaces *> p)

symbol :: Parser Stx.Symbol
symbol =
  choice
    [ keyword "type" *> spaces *>
      (Stx.SymbolType <$> possibleAlias (escapable typeIdent'))
    , keyword "interaction" *> spaces *>
      (Stx.SymbolInteraction <$> possibleAlias (escapable typeIdent'))
    , keyword "protocol" *> spaces *>
      (Stx.SymbolProtocol <$> possibleAlias (escapable typeIdent'))
    , Stx.SymbolDef <$> possibleAlias (escapable varIdent')
    ]

symbols :: Parser Stx.Symbols
symbols =
  choice
    [ char '{' *> spaces *>
      choice
        [ keyword "everything" *> spaces *> optional (char ';') *> pure Stx.SymbolsEverything
        , Stx.SymbolsSpecific <$> semicolonDelimited symbol
        ]
      <* spaces <* char '}'
    , char ';' *> pure (Stx.SymbolsSpecific [])
    ]

import_ :: Parser Stx.Import
import_ =
  keyword "import" *> spaces *>
    (Stx.Import
      <$> (possibleAlias (Stx.Path <$> path <*> escapable moduleIdent') <* spaces)
      <*> symbols
    )

module_ :: Parser Stx.Module
module_ =
  spaces *>
  (Stx.Module
    <$> (keyword "export" *> spaces *> symbols <* spaces)
    <*> many (import_ <* spaces)
    <*> many (decl <* spaces)
  )
