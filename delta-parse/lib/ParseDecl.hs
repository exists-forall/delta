module ParseDecl
  ( decl
  )
where

import ParseUtils

import Data.Text (Text)

import qualified Syntax as Stx
import ParseIdent (ident, keyword)
import ParsePat (typedPat)
import ParseType (possibleFunc, possibleInters)
import ParseExpr (body)

import Data.Bifunctor (first, second, bimap)

markDecl :: Parser Stx.Decl -> Parser Stx.Decl
markDecl p = Stx.MarkDecl <$> getPosition <*> p <*> getPosition

pairZip :: (a -> a' -> a'') -> (b -> b' -> b'') -> (a, b) -> (a', b') -> (a'', b'')
pairZip f g (a, b) (a', b') = (f a a', g b b')

tupleBoth :: (Stx.Pat, Stx.Type) -> (Stx.Pat, Stx.Type) -> (Stx.Pat, Stx.Type)
tupleBoth = pairZip Stx.PatTuple Stx.TypeTuple

extractType :: Stx.TypedPat -> (Stx.Pat, Stx.Type)
extractType (Stx.PatVar v t) = (Stx.PatVar v Nothing, t)
extractType (Stx.PatIgnore t) = (Stx.PatIgnore Nothing, t)
extractType Stx.PatUnit = (Stx.PatUnit, Stx.TypeUnit)
extractType (Stx.MarkPat pos1 p pos2) = first (flip (Stx.MarkPat pos1) pos2) (extractType p)
extractType (Stx.PatTuple a b) = tupleBoth (extractType a) (extractType b)

sigSlot :: Parser Stx.TypedPat
sigSlot = char '(' *> spaces *> option Stx.PatUnit typedPat <* spaces <* char ')'

sigTail :: Parser (Stx.VarIdentTail, [Stx.TypedPat])
sigTail =
  choice
    [ first <$> (Stx.TailWord <$> ident) <*> (spaces *> sigTail)
    , bimap Stx.TailSlot <$> ((:) <$> sigSlot) <*> (spaces *> sigTail)
    , pure (Stx.EmptyTail, [])
    ]

sigBody :: Parser (Stx.VarIdentBody, [Stx.TypedPat])
sigBody =
  choice
    [ first <$> (Stx.BodyWord <$> ident) <*> (spaces *> sigBody)
    , bimap Stx.BodySlot <$> ((:) <$> sigSlot) <*> (spaces *> sigTail)
    ]

sigLhsNonDot :: Parser (Stx.VarIdent, [Stx.TypedPat])
sigLhsNonDot =
  first <$> (Stx.VarIdent <$> ident) <*> (spaces *> sigBody)

sigPostDot :: Parser (Stx.VarIdent, [Stx.TypedPat])
sigPostDot =
  char '.' *> spaces *> (first <$> (Stx.DotVarIdent <$> ident) <*> (spaces *> sigTail))

sigLhsDot :: Parser (Stx.VarIdent, [Stx.TypedPat])
sigLhsDot =
  second <$> ((:) <$> sigSlot) <*> (spaces *> sigPostDot)

sigLhs :: Parser (Stx.VarIdent, (Stx.Pat, Stx.Type))
sigLhs =
  second (foldr1 tupleBoth . map extractType) <$>
    choice
      [ sigLhsNonDot
      , sigLhsDot
      ]

sigInterType :: Parser Stx.Type
sigInterType =
  option Stx.TypePure $ char '!' *> spaces *> possibleInters

sigRetType :: Parser Stx.Type
sigRetType =
  option Stx.TypeUnit $ string "->" *> spaces *> possibleFunc

assembleSig ::
  (Stx.VarIdent, (Stx.Pat, Stx.Type)) ->
  Stx.Type ->
  Stx.Type ->
  (Stx.VarIdent, (Stx.Pat, Stx.Type))

assembleSig (name, (lhsPat, argType)) interType retType =
  (name, (lhsPat, Stx.TypeFunc argType interType retType))

sig :: Parser (Stx.VarIdent, (Stx.Pat, Stx.Type))
sig = assembleSig <$> sigLhs <*> (spaces *> sigInterType <* spaces) <*> sigRetType

assembleDef :: (Stx.VarIdent, (Stx.Pat, Stx.Type)) -> Stx.Expr -> Stx.Decl
assembleDef (name, (argPat, funcType)) bodyExpr =
  Stx.DeclDef (Stx.PatVar name funcType) (Stx.Func argPat bodyExpr)

def :: Parser Stx.Decl
def =
  markDecl $
  keyword "def" *> spaces *>
    (assembleDef <$> sig <*> (spaces *> char '{' *> spaces *> body <* spaces <* char '}'))

decl :: Parser Stx.Decl
decl =
  choice
    [ def
    ]
