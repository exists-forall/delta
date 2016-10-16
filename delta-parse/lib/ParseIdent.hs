{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module ParseIdent
  -- Common combinators with implicit reserved word handling
  ( ident
  , path
  , escapableIdent
  , escapable
  , keyword
  , operatorIdent
  , prefixOperatorIdent

  -- Lower-level combinators with explicit reserved word handling
  , CheckReserved (..)
  , ident'
  , moduleIdent'
  , varIdentNonDotWithSlot'
  , varIdentDotSuffixWithSlot'
  , varIdent'
  , typeIdent'
  , typeVarIdent'
  )
where

import ParseUtils

import qualified Syntax as Stx

import Data.Bifunctor (first, bimap)

reservedWord :: Parser String
reservedWord =
  choice $ map (try . (<* notFollowedBy identChar) . string)
    [ "do"
    , "Pure"
    , "Never"
    , "def"
    , "type"
    , "case"
    , "protocol"
    , "implement"
    , "where"
    , "interaction"
    , "message"
    , "export"
    , "import"
    , "everything"
    ]

data CheckReserved = AllowReserved | ForbidReserved

upperLetter :: Parser Stx.Letter
upperLetter = rangeParser "uppercase letter" 'A' 'Z'

lowerLetter :: Parser Stx.Letter
lowerLetter = rangeParser "lowercase letter" 'a' 'z'

syntaxDigit :: Parser Stx.Digit
syntaxDigit = rangeParser "digit" '0' '9'

syntaxLetter :: Parser (Stx.Capitalization, Stx.Letter)
syntaxLetter = ((Stx.LowerCase,) <$> lowerLetter) <|> ((Stx.UpperCase,) <$> upperLetter) <?> "letter"

identStartChar :: Parser Stx.IdentStartChar
identStartChar =
  ((uncurry Stx.Alpha) <$> syntaxLetter) <|>
  ((char '_' *> pure Stx.Underscore) <?> "underscore")

identChar :: Parser Stx.IdentChar
identChar = (Stx.StartChar <$> identStartChar) <|> (Stx.Digit <$> syntaxDigit)

checkReserved :: CheckReserved -> Parser a -> Parser a
checkReserved AllowReserved p = p
checkReserved ForbidReserved p = notFollowedBy reservedWord *> p

ident' :: CheckReserved -> Parser Stx.IdentText
ident' reserved =
  flip label "identifier" $
  try $ checkReserved reserved $
  Stx.identText <$> (Stx.Ident <$> identStartChar <*> many identChar)

ident :: Parser Stx.IdentText
ident = ident' ForbidReserved

moduleIdent' :: CheckReserved -> Parser Stx.ModuleIdentText
moduleIdent' reserved =
  flip label "module identifier" $
  try $ checkReserved reserved $
  Stx.moduleIdentText <$> (Stx.ModuleIdent <$> upperLetter <*> many identChar)

varIdentTailWithSlot' :: CheckReserved -> Parser a -> Parser (Stx.VarIdentTail, [a])
varIdentTailWithSlot' reserved slot =
  choice
    [ first
      <$> (Stx.TailWord <$> ident' reserved)
      <*> (spaces *> varIdentTailWithSlot' reserved slot)
    , bimap Stx.TailSlot <$> ((:) <$> slot) <*> (spaces *> varIdentTailWithSlot' reserved slot)
    , pure (Stx.EmptyTail, [])
    ]

varIdentBodyWithSlot' :: CheckReserved -> Parser a -> Parser (Stx.VarIdentBody, [a])
varIdentBodyWithSlot' reserved slot =
  choice
    [ first
      <$> (Stx.BodyWord <$> ident' reserved)
      <*> (spaces *> varIdentBodyWithSlot' reserved slot)
    , bimap Stx.BodySlot <$> ((:) <$> slot) <*> (spaces *> varIdentTailWithSlot' reserved slot)
    ]

varIdentNonDotWithSlot' :: CheckReserved -> Parser a -> Parser (Stx.VarIdent, [a])
varIdentNonDotWithSlot' reserved slot =
  first <$> (Stx.VarIdent <$> ident' reserved) <*> (spaces *> varIdentBodyWithSlot' reserved slot)

{- NOTE:
DOES NOT INCLUDE THE DOT ITSELF!
e.g. matches the 'foo' in '.foo' or the 'bar () baz ()' in '.A::B::bar () baz ()'
-}
varIdentDotSuffixWithSlot' :: CheckReserved -> Parser a -> Parser (Stx.VarIdent, [a])
varIdentDotSuffixWithSlot' reserved slot =
 first <$> (Stx.DotVarIdent <$> ident' reserved) <*> (spaces *> varIdentTailWithSlot' reserved slot)

trivialSlot :: Parser ()
trivialSlot = char '(' *> spaces <* char ')'

varIdent' :: CheckReserved -> Parser Stx.VarIdent
varIdent' reserved =
  choice
    [ try $ fst <$> varIdentNonDotWithSlot' reserved trivialSlot
    , flip Stx.VarIdent (Stx.BodySlot Stx.EmptyTail) <$> ident' reserved
    , try $ char '.' *> spaces *> (fst <$> varIdentDotSuffixWithSlot' reserved trivialSlot)
    , try $
      (Stx.PrefixOperatorIdent <$> prefixOperatorIdent) <* spaces <* char '(' <* spaces <* char ')'
    , try $ Stx.OperatorIdent <$> operatorIdent
    ]

operatorIdent :: Parser Stx.OperatorIdent
operatorIdent =
  choice
    [ char '+' *> pure Stx.OpAdd
    , char '-' *> pure Stx.OpSub
    , char '*' *> pure Stx.OpMul
    , char '/' *> pure Stx.OpDiv

    , try (string "==") *> pure Stx.OpEqu
    , try (string "=/=") *> pure Stx.OpNotEqu
    , try (string ">=") *> pure Stx.OpGTE
    , try (string "<=") *> pure Stx.OpLTE

    , try (string "<<") *> pure Stx.OpCompLeft
    , try (string ">>") *> pure Stx.OpCompRight

    , char '<' *> pure Stx.OpLT
    , char '>' *> pure Stx.OpGT

    , string "&&" *> pure Stx.OpAnd
    , try (string "||") *> pure Stx.OpOr -- `try` to prevent ambiguities with closure arguments

    , char '@' *> pure Stx.OpAt
    ]

prefixOperatorIdent :: Parser Stx.PrefixOperatorIdent
prefixOperatorIdent =
  choice
    [ char '-' *> pure Stx.OpNegate
    , char '~' *> pure Stx.OpNot
    ]

typeIdent' :: CheckReserved -> Parser Stx.TypeIdentText
typeIdent' reserved =
  flip label "type identifier" $
  try $ checkReserved reserved $
  Stx.typeIdentText <$> (Stx.TypeIdent <$> upperLetter <*> many identChar)

typeVarIdent' :: CheckReserved -> Parser Stx.TypeVarIdentText
typeVarIdent' reserved =
  flip label "type variable" $
  try $ checkReserved reserved $
  Stx.typeVarIdentText <$> (Stx.TypeVarIdent <$> lowerLetter <*> many identChar)

path :: Parser [Stx.ModuleIdentText]
path =
  many $ try $ escapable moduleIdent' <* spaces <* char ':' <* char ':' <* spaces

escapableIdent :: Parser Stx.VarIdent
escapableIdent =
  choice
    [ flip Stx.VarIdent (Stx.BodySlot Stx.EmptyTail) <$> ident' ForbidReserved
    , try $ char '`' *> spaces *> ParseIdent.varIdent' AllowReserved <* spaces <* char '`'
    ]

escapable :: (CheckReserved -> Parser a) -> Parser a
escapable p =
  choice
    [ p ForbidReserved
    , try $ char '`' *> spaces *> p AllowReserved <* spaces <* char '`'
    ]

keyword :: String -> Parser String
keyword word =
  try (string word <* notFollowedBy identChar)
  <?> ("Keyword \"" ++ word ++ "\"")
