module ParseModule
  (module_
  )
where

import ParseUtils

import Data.Text (Text)

import qualified Syntax as Stx
import ParseIdent
import ParseDecl

assemblePossibleAlias :: a -> Maybe a -> Stx.PossibleAlias a
assemblePossibleAlias alias (Just original) = Stx.Alias alias original
assemblePossibleAlias original Nothing = Stx.NoAlias original

possibleAlias :: Parser a -> Parser (Stx.PossibleAlias a)
possibleAlias p =
  assemblePossibleAlias <$> (p <* spaces) <*> optionMaybe (char '=' *> spaces *> p)

exportSymbol :: Parser Stx.ExportSymbol
exportSymbol =
  choice
    [ keyword "type" *> spaces *>
      (Stx.ExportType <$> possibleAlias (escapable typeIdent'))
    , keyword "interaction" *> spaces *>
      (Stx.ExportInteraction <$> possibleAlias (escapable typeIdent'))
    , keyword "protocol" *> spaces *>
      (Stx.ExportProtocol <$> possibleAlias (escapable typeIdent'))
    , Stx.ExportDef <$> possibleAlias (escapable varIdent')
    ]

exportSymbols :: Parser Stx.ExportSymbols
exportSymbols =
  choice
    [ char '{' *> spaces *>
      choice
        [ keyword "everything" *> spaces *> optional (char ';') *> pure Stx.ExportEverything
        , Stx.ExportSpecific <$> semicolonDelimited exportSymbol
        ]
      <* spaces <* char '}'
    , char ';' *> pure (Stx.ExportSpecific [])
    ]

import_ :: Parser Stx.Import
import_ =
  keyword "import" *> spaces *>
    (Stx.Import
      <$> (possibleAlias (Stx.Path <$> path <*> escapable moduleIdent') <* spaces)
      <*> exportSymbols
    )

module_ :: Parser Stx.Module
module_ =
  spaces *>
  (Stx.Module
    <$> (keyword "export" *> spaces *> exportSymbols <* spaces)
    <*> many (import_ <* spaces)
    <*> many (decl <* spaces)
  )
