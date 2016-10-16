module ParsePat
  ( pat
  , typedPat
  )
where

import ParseUtils

import qualified Syntax as Stx
import ParseIdent (escapableIdent)
import ParseType (possibleFunc)

markPat :: Parser (Stx.Pat' annot) -> Parser (Stx.Pat' annot)
markPat p = Stx.MarkPat <$> getPosition <*> p <*> getPosition

patVar :: Parser annot -> Parser (Stx.Pat' annot)
patVar annot =
  markPat $
  Stx.PatVar
    <$> (Stx.varIdentText <$> escapableIdent <* spaces)
    <*> annot

patIgnore :: Parser annot -> Parser (Stx.Pat' annot)
patIgnore annot = markPat $ Stx.PatIgnore <$> (char '_' *> spaces *> annot)

parenthesizedPat :: Parser annot -> Parser (Stx.Pat' annot)
parenthesizedPat annot =
  markPat $
  char '(' *> spaces *> option Stx.PatUnit (pat' annot) <* spaces <* char ')'

atomicPat :: Parser annot -> Parser (Stx.Pat' annot)
atomicPat annot =
  choice
    [ parenthesizedPat annot
    , patIgnore annot
    , patVar annot
    ]

patTuple :: Parser annot -> Parser (Stx.Pat' annot)
patTuple annot =
  markPat $
  foldr1 Stx.PatTuple <$> sepBy1 (atomicPat annot <* spaces) (char ',' <* spaces)

pat' :: Parser annot -> Parser (Stx.Pat' annot)
pat' annot = patTuple annot

typeAnnotation :: Parser Stx.Type
typeAnnotation = char ':' *> spaces *> possibleFunc

pat :: Parser Stx.Pat
pat = pat' $ optionMaybe typeAnnotation

typedPat :: Parser Stx.TypedPat
typedPat = pat' typeAnnotation
