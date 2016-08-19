module ParseExpr
  ( expr
  )
where

import ParseUtils

import qualified Syntax as Stx
import ParseIdent

import Data.Bifunctor (bimap, first)

mark :: Parser Stx.Expr -> Parser Stx.Expr
mark p = Stx.Mark <$> getPosition <*> p <*> getPosition

parenthesized :: Parser Stx.Expr
parenthesized = mark $ char '(' *> spaces *> option Stx.Unit expr <* spaces <* char ')'

callTail :: Parser (Stx.VarIdentTail, [Stx.Expr])
callTail =
  choice
    [ first <$> (Stx.TailWord <$> ident) <*> (spaces *> callTail)
    , bimap Stx.TailSlot <$> ((:) <$> parenthesized) <*> (spaces *> callTail)
    , pure (Stx.EmptyTail, [])
    ]

callBody :: Parser (Stx.VarIdentBody, [Stx.Expr])
callBody =
  choice
    [ first <$> (Stx.BodyWord <$> ident) <*> (spaces *> callBody)
    , bimap Stx.BodySlot <$> ((:) <$> parenthesized) <*> (spaces *> callTail)
    ]

callNonDot :: Parser Stx.Expr
callNonDot =
  mark $ assemble <$> path <*> (first <$> (Stx.VarIdent <$> ident) <*> (spaces *> callBody)) where
  assemble varPath (varIdent, args) =
    Stx.Call (Stx.Var (Stx.Path varPath varIdent)) (foldr1 Stx.Tuple args)

var :: Parser Stx.Expr
var = mark $
  Stx.Var <$> (Stx.Path <$> path <*> (flip Stx.VarIdent (Stx.BodySlot Stx.EmptyTail) <$> ident))

atomicExpr :: Parser Stx.Expr
atomicExpr = choice
  [ parenthesized
  , try callNonDot
  , var
  ]

dotCall :: Parser (Stx.Path Stx.VarIdent, [Stx.Expr])
dotCall =
  char '.' *> spaces *>
  (first <$> ((.) <$> (Stx.Path <$> path) <*> (Stx.DotVarIdent <$> ident)) <*> (spaces *> callTail))

applyDotCall :: Stx.Expr -> (Stx.Path Stx.VarIdent, [Stx.Expr]) -> Stx.Expr
applyDotCall receiver (varIdent, args) =
  Stx.Call (Stx.Var varIdent) (foldr1 Stx.Tuple (receiver : args))

dotCalls :: Parser Stx.Expr
dotCalls =
  {-
  Spaces must belong to the *end* of elements here, not the beginning, because if spaces are
  consumed at the beginning of an element then that element will be considered to have consumed
  input and will become non-negotiable.

  In other words, if spaces were considered to belong the beginning of dot calls, then if there
  were trailing spaces after the last dot call, Parsec would treat those spaces as the beginning of
  another (spurious and nonexistent) dot call, try to parse it, and fail.
  -}
  -- TODO: It would be nice to mark each chained dot call individually
  mark $ foldl applyDotCall <$> (atomicExpr <* spaces) <*> many (dotCall <* spaces)

expr :: Parser Stx.Expr
expr = dotCalls
