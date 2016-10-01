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
import ParseExpr (body, expr)

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

data Sig slot = Sig Stx.VarIdent [slot] Stx.Type Stx.Type [Stx.Constraint]

type Def = (Stx.TypedPat, [Stx.Constraint], Stx.Expr)

sigArgs :: Parser slot -> Parser (Stx.VarIdent, [slot])
sigArgs slot =
  choice
    [ varIdentNonDotWithSlot' ForbidReserved slot
    , (,) <$> (Stx.PrefixOperatorIdent <$> prefixOperatorIdent <* spaces) <*> ((:[]) <$> slot)
    , second
      <$> ((:) <$> slot <* spaces)
      <*> choice
        [ char '.' *> spaces *> varIdentDotSuffixWithSlot' ForbidReserved slot
        , (,) <$> (Stx.OperatorIdent <$> operatorIdent <* spaces) <*> ((:[]) <$> slot)
        ]
    ]

sigInterType :: Parser Stx.Type
sigInterType =
  option Stx.TypePure $ char '!' *> spaces *> possibleInters

sigRetType :: Parser Stx.Type
sigRetType =
  option Stx.TypeUnit $ string "->" *> spaces *> possibleFunc

constraint :: Parser Stx.Constraint
constraint =
  Stx.ConstraintImplement
    <$> (Stx.Path <$> path <*> escapable typeIdent')
    <*> (spaces *> char '<' *> spaces *> type_ <* spaces <* char '>')

whereClause :: Parser [Stx.Constraint]
whereClause =
  option [] $ keyword "where" *> spaces *> sepByTrailing (constraint <* spaces) (char ',' *> spaces)

sig :: Parser slot -> Parser (Sig slot)
sig slot =
  uncurry Sig
    <$> (sigArgs slot <* spaces)
    <*> (sigInterType <* spaces)
    <*> (sigRetType <* spaces)
    <*> whereClause

defSlot :: Parser Stx.TypedPat
defSlot = char '(' *> spaces *> option Stx.PatUnit typedPat <* spaces <* char ')'

assembleDef :: Sig Stx.TypedPat -> Stx.Expr -> Def
assembleDef (Sig name args interType retType constraints) bodyExpr =
  let
    (argPat, argType) = foldr1 tupleBoth $ map extractType args
    funcType = Stx.TypeFunc argType interType retType
  in
    (Stx.PatVar name funcType, constraints, Stx.Func argPat bodyExpr)

funcDef :: Parser Def
funcDef =
  keyword "def" *> spaces *>
    (assembleDef <$> sig defSlot <*> (spaces *> char '{' *> spaces *> body <* spaces <* char '}'))

constDef :: Parser Def
constDef =
  (,,)
    <$> (typedPat <* spaces)
    <*> (whereClause <* spaces)
    <*> (char '=' *> spaces *> expr <* spaces <* char ';')

def :: Parser Def
def =
  choice
    [ funcDef
    , constDef
    ]

defToDecl :: Def -> Stx.Decl
defToDecl (pat, constraints, funcExpr) = Stx.DeclDef pat constraints funcExpr

defDecl :: Parser Stx.Decl
defDecl = markDecl $ defToDecl <$> def

stubDefSlot :: Parser Stx.Type
stubDefSlot = char '(' *> spaces *> option Stx.TypeUnit type_ <* spaces <* char ')'

assembleStubDef :: Sig Stx.Type -> Stx.Stub
assembleStubDef (Sig name argType interType retType constraints) =
  Stx.StubDef name (Stx.TypeFunc (foldr1 Stx.TypeTuple argType) interType retType) constraints

stubFuncDef :: Parser Stx.Stub
stubFuncDef =
  keyword "def" *> spaces *> (assembleStubDef <$> sig stubDefSlot) <* spaces <* char ';'

stubConstDef :: Parser Stx.Stub
stubConstDef =
  (Stx.StubDef
    <$> (escapableIdent <* spaces)
    <*> (char ':' *> spaces *> possibleFunc <* spaces)
    <*> (whereClause <* spaces)
  ) <* char ';'

stubImplement :: Parser Stx.Stub
stubImplement =
  keyword "implement" *> spaces *>
  (Stx.StubImplement
    <$> ((Stx.Path <$> path <*> escapable typeIdent') <* spaces)
    <*> (char '<' *> spaces *> type_ <* spaces <* char '>' <* spaces)
    <*> (whereClause <* spaces)
  ) <* char ';'

stub :: Parser Stx.Stub
stub =
  choice
    [ stubFuncDef
    , stubImplement
    , stubConstDef
    ]

protocol :: Parser Stx.Decl
protocol =
  markDecl $
  Stx.DeclProtocol
    <$> (keyword "protocol" *> spaces *> escapable typeIdent' <* spaces)
    <*> (char '<' *> spaces *> escapable typeVarIdent' <* spaces <* char '>' <* spaces)
    <*> (char '{' *> spaces *> many (stub <* spaces) <* char '}')

implement :: Parser Stx.Decl
implement =
  markDecl $
  Stx.DeclImplement
    <$> (keyword "implement" *> spaces *> (Stx.Path <$> path <*> escapable typeIdent') <* spaces)
    <*> (char '<' *> spaces *> type_ <* spaces <* char '>' <* spaces)
    <*> (whereClause <* spaces)
    <*> (char '{' *> spaces *> many (def <* spaces) <* char '}')

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

message :: Parser (Stx.Ident, Stx.Type, Stx.Type)
message =
  keyword "message" *> spaces *>
    ((,,)
      <$> (escapable ident' <* spaces)
      <*> (char '(' *> spaces *> type_ <* spaces <* char ')' <* spaces)
      <*> (string "->" *> spaces *> possibleFunc <* spaces)
    ) <* char ';'

interaction :: Parser Stx.Decl
interaction =
  keyword "interaction" *> spaces *>
    (Stx.DeclInteraction
      <$> (escapable typeIdent' <* spaces)
      <*> many (char '<' *> spaces *> escapable typeVarIdent' <* spaces <* char '>' <* spaces)
      <*> (char '{' *> spaces *> many (message <* spaces) <* spaces <* char '}')
    )

decl :: Parser Stx.Decl
decl =
  choice
    [ defDecl
    , typeStruct
    , protocol
    , implement
    , interaction
    ]
