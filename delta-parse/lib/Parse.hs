{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Parse
  ( ParseResult (..)
  , parseModule
  , parseDecl
  , parseExpr
  , parseType
  )
where

import GHC.Generics (Generic)

import Data.Text.Lazy (Text)
import Data.Aeson.Types (FromJSON, ToJSON)

import ParseUtils
import qualified Delta.Structures.Syntax as Stx
import ParseModule (module_)
import ParseDecl (decl)
import ParseExpr (expr)
import ParseType (type_)
import StripRedundantMarks

data ParseResult a
  = ParseSuccess
    { parsed_module :: a
    }
  | ParseError
    { error_message :: String
    , error_source_pos :: SourcePos
    }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

wrapParser :: Parser a -> (a -> a) -> Text -> ParseResult a
wrapParser p strip src =
  case fullParse (spaces *> p <* spaces) src of
    Right result -> ParseSuccess (strip result)
    Left err -> ParseError (show err) (fromParsecSourcePos $ errorPos err)

parseModule :: Text -> ParseResult Stx.Module
parseModule = wrapParser module_ stripModuleMarks

parseDecl :: Text -> ParseResult Stx.Decl
parseDecl = wrapParser decl stripDeclMarks

parseExpr :: Text -> ParseResult Stx.Expr
parseExpr = wrapParser expr stripMarks

parseType :: Text -> ParseResult Stx.Type
parseType = wrapParser type_ stripTypeMarks
