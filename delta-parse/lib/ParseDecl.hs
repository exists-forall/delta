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

import Data.Bifunctor (first, second)

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

data Sig slot = Sig Stx.VarIdent [slot] Stx.Type Stx.Type

sigArgs :: Parser slot -> Parser (Stx.VarIdent, [slot])
sigArgs slot =
  choice
    [ varIdentNonDotWithSlot' ForbidReserved slot
    , second
      <$> ((:) <$> slot)
      <*> (spaces *> char '.' *> spaces *> varIdentDotSuffixWithSlot' ForbidReserved slot)
    ]

sigInterType :: Parser Stx.Type
sigInterType =
  option Stx.TypePure $ char '!' *> spaces *> possibleInters

sigRetType :: Parser Stx.Type
sigRetType =
  option Stx.TypeUnit $ string "->" *> spaces *> possibleFunc

sig :: Parser slot -> Parser (Sig slot)
sig slot = uncurry Sig <$> (sigArgs slot <* spaces) <*> (sigInterType <* spaces) <*> sigRetType

defSlot :: Parser Stx.TypedPat
defSlot = char '(' *> spaces *> option Stx.PatUnit typedPat <* spaces <* char ')'

assembleDef :: Sig Stx.TypedPat -> Stx.Expr -> Stx.Decl
assembleDef (Sig name args interType retType) bodyExpr =
  let
    (argPat, argType) = foldr1 tupleBoth $ map extractType args
    funcType = Stx.TypeFunc argType interType retType
  in
    Stx.DeclDef (Stx.PatVar name funcType) (Stx.Func argPat bodyExpr)

def :: Parser Stx.Decl
def =
  markDecl $
  keyword "def" *> spaces *>
    (assembleDef <$> sig defSlot <*> (spaces *> char '{' *> spaces *> body <* spaces <* char '}'))

stubDefSlot :: Parser Stx.Type
stubDefSlot = char '(' *> spaces *> option Stx.TypeUnit type_ <* spaces <* char ')'

assembleStubDef :: Sig Stx.Type -> Stx.Stub
assembleStubDef (Sig name argType interType retType) =
  Stx.StubDef name $ Stx.TypeFunc (foldr1 Stx.TypeTuple argType) interType retType

stubDef :: Parser Stx.Stub
stubDef =
  keyword "def" *> spaces *> (assembleStubDef <$> sig stubDefSlot) <* spaces <* char ';'

stubImplement :: Parser Stx.Stub
stubImplement =
  keyword "implement" *> spaces *>
  (Stx.StubImplement
    <$> (escapable typeIdent' <* spaces)
    <*> (char '<' *> spaces *> type_ <* spaces <* char '>')
  ) <* spaces <* char ';'

stub :: Parser Stx.Stub
stub =
  choice
    [ stubDef
    , stubImplement
    ]

protocol :: Parser Stx.Decl
protocol =
  markDecl $
  Stx.DeclProtocol
    <$> (keyword "protocol" *> spaces *> escapable typeIdent' <* spaces)
    <*> (char '<' *> spaces *> escapable typeVarIdent' <* spaces <* char '>' <* spaces)
    <*> (char '{' *> spaces *> many (stub <* spaces) <* char '}')

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
    , protocol
    ]
