{-# LANGUAGE TupleSections #-}

module ParseIdent
  ( ident
  , moduleIdent
  , varIdent
  , typeIdent
  )
where

import ParseUtils

import qualified Syntax as Stx

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

ident :: Parser Stx.Ident
ident =
  flip label "identifier" $
  try $
  Stx.Ident <$> identStartChar <*> many identChar

moduleIdent :: Parser Stx.ModuleIdent
moduleIdent =
  flip label "module identifier" $
  try $
  Stx.ModuleIdent <$> upperLetter <*> many identChar

varIdentTail :: Parser Stx.VarIdentTail
varIdentTail =
  (Stx.TailWord <$> ident <*> (spaces *> varIdentTail)) <|>
  (Stx.TailSlot <$> (char '(' *> spaces *> char ')' *> spaces *> varIdentTail)) <|>
  pure Stx.EmptyTail

varIdentBody :: Parser Stx.VarIdentBody
varIdentBody =
  (Stx.BodyWord <$> ident <*> (spaces *> varIdentBody)) <|>
  (Stx.BodySlot <$> (char '(' *> spaces *> char ')' *> spaces *> varIdentTail))

varIdent :: Parser Stx.VarIdent
varIdent =
  choice
    [ try $ Stx.VarIdent <$> ident <*> (spaces *> varIdentBody)
    , (flip Stx.VarIdent (Stx.BodySlot Stx.EmptyTail)) <$> ident
    , try $ char '.' *> spaces *> (Stx.DotVarIdent <$> ident <*> (spaces *> varIdentTail))
    ]

typeIdent :: Parser Stx.TypeIdent
typeIdent =
  flip label "type identifier" $
  try $
  Stx.TypeIdent <$> upperLetter <*> many identChar
