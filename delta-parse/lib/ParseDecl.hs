module ParseDecl
  ( decl
  )
where

import ParseUtils

import Data.Text (Text)

import qualified Syntax as Stx
import ParseIdent
import ParsePat (typedPat)
import ParseType (possibleFunc, possibleInters, type_)
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

sigLhsDot :: Parser (Stx.VarIdent, [Stx.TypedPat])
sigLhsDot =
  second <$> ((:) <$> sigSlot) <*>
    (spaces *> char '.' *> spaces *> varIdentDotSuffixWithSlot' ForbidReserved sigSlot)

sigLhs :: Parser (Stx.VarIdent, (Stx.Pat, Stx.Type))
sigLhs =
  second (foldr1 tupleBoth . map extractType) <$>
    choice
      [ varIdentNonDotWithSlot' ForbidReserved sigSlot
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

caseBody :: Parser [Stx.StructComponent]
caseBody =
  (char ';' *> pure []) <|> structBody

structCase :: Parser Stx.StructComponent
structCase =
  keyword "case" *> (Stx.StructCase <$> (spaces *> escapable typeIdent' <* spaces) <*> caseBody)

structField :: Parser Stx.StructComponent
structField =
  Stx.StructField
    <$> (escapable ident' <* spaces)
    <*> (char ':' *> spaces *> type_ <* spaces <* char ';')

structComponent :: Parser Stx.StructComponent
structComponent = structCase <|> structField

structBody :: Parser [Stx.StructComponent]
structBody = char '{' *> spaces *> many (structComponent <* spaces) <* spaces <* char '}'

typeStruct :: Parser Stx.Decl
typeStruct =
  markDecl $
  keyword "type" *>
    (Stx.DeclTypeStruct
      <$> (spaces *> escapable typeIdent' <* spaces)
      <*> (many $ char '<' *> spaces *> escapable typeVarIdent' <* spaces <* char '>' <* spaces)
      <*> structBody)

decl :: Parser Stx.Decl
decl =
  choice
    [ def
    , typeStruct
    ]
