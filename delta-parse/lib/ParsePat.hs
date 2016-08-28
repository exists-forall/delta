module ParsePat
  ( pat
  )
where

import ParseUtils

import qualified Syntax as Stx
import ParseIdent (escapableIdent)
import ParseType (possibleFunc)

markPat :: Parser Stx.Pat -> Parser Stx.Pat
markPat p = Stx.MarkPat <$> getPosition <*> p <*> getPosition

patVar :: Parser Stx.Pat
patVar =
  markPat $
  Stx.PatVar
    <$> (escapableIdent <* spaces)
    <*> optionMaybe (char ':' *> spaces *> possibleFunc)

patIgnore :: Parser Stx.Pat
patIgnore = markPat $ char '_' *> pure Stx.PatIgnore

parenthesizedPat :: Parser Stx.Pat
parenthesizedPat = markPat $ char '(' *> spaces *> option Stx.PatUnit pat <* spaces <* char ')'

atomicPat :: Parser Stx.Pat
atomicPat =
  choice
    [ parenthesizedPat
    , patIgnore
    , patVar
    ]

patTuple :: Parser Stx.Pat
patTuple = markPat $ foldr1 Stx.PatTuple <$> sepBy1 (atomicPat <* spaces) (char ',' <* spaces)

pat :: Parser Stx.Pat
pat = patTuple
