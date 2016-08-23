{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module ParseIdent
  -- Common combinators with implicit reserved word handling
  ( ident
  , path
  , escapableIdent
  , keyword

  -- Lower-level combinators with explicit reserved word handling
  , CheckReserved (..)
  , ident'
  , moduleIdent'
  , varIdent'
  , typeIdent'
  , path'
  )
where

import ParseUtils

import qualified Syntax as Stx

reservedWord :: Parser String
reservedWord =
  choice $ map (try . (<* notFollowedBy identChar) . string)
    [ "do"
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

ident' :: CheckReserved -> Parser Stx.Ident
ident' reserved =
  flip label "identifier" $
  try $ checkReserved reserved $
  Stx.Ident <$> identStartChar <*> many identChar

ident :: Parser Stx.Ident
ident = ident' ForbidReserved

moduleIdent' :: CheckReserved -> Parser Stx.ModuleIdent
moduleIdent' reserved =
  flip label "module identifier" $
  try $ checkReserved reserved $
  Stx.ModuleIdent <$> upperLetter <*> many identChar

varIdentTail' :: CheckReserved -> Parser Stx.VarIdentTail
varIdentTail' reserved =
  (Stx.TailWord <$> ident' reserved <*> (spaces *> varIdentTail' reserved)) <|>
  (Stx.TailSlot <$> (char '(' *> spaces *> char ')' *> spaces *> varIdentTail' reserved)) <|>
  pure Stx.EmptyTail

varIdentBody' :: CheckReserved -> Parser Stx.VarIdentBody
varIdentBody' reserved =
  (Stx.BodyWord <$> ident' reserved <*> (spaces *> varIdentBody' reserved)) <|>
  (Stx.BodySlot <$> (char '(' *> spaces *> char ')' *> spaces *> varIdentTail' reserved))

varIdent' :: CheckReserved -> Parser Stx.VarIdent
varIdent' reserved =
  choice
    [ try $ Stx.VarIdent <$> ident' reserved <*> (spaces *> varIdentBody' reserved)
    , (flip Stx.VarIdent (Stx.BodySlot Stx.EmptyTail)) <$> ident' reserved
    , try $ char '.' *> spaces *>
      (Stx.DotVarIdent <$> ident' reserved <*> (spaces *> varIdentTail' reserved))
    ]

typeIdent' :: CheckReserved -> Parser Stx.TypeIdent
typeIdent' reserved =
  flip label "type identifier" $
  try $ checkReserved reserved $
  Stx.TypeIdent <$> upperLetter <*> many identChar

path' :: CheckReserved -> Parser [Stx.ModuleIdent]
path' reserved = many (try $ moduleIdent' reserved <* spaces <* char ':' <* char ':' <* spaces)

path :: Parser [Stx.ModuleIdent]
path = path' ForbidReserved

escapableIdent :: Parser Stx.VarIdent
escapableIdent =
  choice
    [ flip Stx.VarIdent (Stx.BodySlot Stx.EmptyTail) <$> ident' ForbidReserved
    , try $ char '`' *> spaces *> ParseIdent.varIdent' AllowReserved <* spaces <* char '`'
    ]

keyword :: String -> Parser String
keyword word =
  try (string word <* notFollowedBy identChar)
  <?> ("Keyword \"" ++ word ++ "\"")
