module ParseExpr
  ( expr
  )
where

import ParseUtils

import Data.Text (Text)

import qualified Syntax as Stx
import ParseIdent (ident, path, escapableIdent)
import ParsePat (pat)
import Precedence
import DeltaPrecedence

import Data.Bifunctor (bimap, first)

mark :: Parser Stx.Expr -> Parser Stx.Expr
mark p = Stx.Mark <$> getPosition <*> p <*> getPosition

parenthesized :: Parser Stx.Expr
parenthesized = mark $ char '(' *> spaces *> option Stx.Unit expr <* spaces <* char ')'

slot :: Parser Stx.Expr
slot =
  choice
    [ parenthesized
    , litString
    , func
    ]

callTail :: Parser (Stx.VarIdentTail, [Stx.Expr])
callTail =
  choice
    [ first <$> (Stx.TailWord <$> ident) <*> (spaces *> callTail)
    , bimap Stx.TailSlot <$> ((:) <$> slot) <*> (spaces *> callTail)
    , pure (Stx.EmptyTail, [])
    ]

callBody :: Parser (Stx.VarIdentBody, [Stx.Expr])
callBody =
  choice
    [ first <$> (Stx.BodyWord <$> ident) <*> (spaces *> callBody)
    , bimap Stx.BodySlot <$> ((:) <$> slot) <*> (spaces *> callTail)
    ]

callNonDot :: Parser Stx.Expr
callNonDot =
  mark $ assemble <$> path <*> (first <$> (Stx.VarIdent <$> ident) <*> (spaces *> callBody)) where
  assemble varPath (varIdent, args) =
    Stx.Call (Stx.Var (Stx.Path varPath varIdent)) (foldr1 Stx.Tuple args)

var :: Parser Stx.Expr
var = mark $ Stx.Var <$> (Stx.Path <$> path <*> escapableIdent)

litUInt :: Parser Stx.Expr
litUInt = mark $ (Stx.LitUInt . read) <$> many1 digit

hexDigitValue :: Parser Int
hexDigitValue =
  choice
    [ rangeParser "digit 0-9" '0' '9'
    , (10 +) <$> rangeParser "letter a-f" 'a' 'f'
    , (10 +) <$> rangeParser "letter A-F" 'A' 'F'
    ]

hexNumber :: Parser Int
hexNumber = foldl ((+) . (16 *)) 0 <$> many1 hexDigitValue

escapeSequence :: Parser Stx.StringComponent
escapeSequence = choice
  [ char '\\' *> pure (Stx.Char '\\')
  , char '"' *> pure (Stx.Char '"')
  , char 'n' *> pure (Stx.Char '\n')
  , char 't' *> pure (Stx.Char '\t')
  , char 'r' *> pure (Stx.Char '\r')
  , char 'u' *> char '{' *> ((Stx.Char . toEnum) <$> hexNumber) <* char '}'
  , Stx.Interpolate <$> parenthesized
  ]

stringComponent :: Parser Stx.StringComponent
stringComponent =
  choice
    [ char '\\' *> escapeSequence
    , Stx.Char <$> satisfy (/= '"')
    ]

litString :: Parser Stx.Expr
litString = mark $
  Stx.LitString <$> (char '"' *> many stringComponent <* char '"')

letStat :: Parser Stx.Stat
letStat =
  Stx.Let <$> try (pat <* spaces <* notFollowedBy operator <* char '=') <*> (spaces *> expr)

assembleEvalOrRet :: Stx.Expr -> Maybe ([Stx.Stat], Stx.Expr) -> ([Stx.Stat], Stx.Expr)
assembleEvalOrRet e (Just (stats, ret)) = (Stx.Let Stx.PatIgnore e : stats, ret)
assembleEvalOrRet e Nothing = ([], e)

markStat :: Parser Stx.Stat -> Parser Stx.Stat
markStat p = Stx.MarkStat <$> getPosition <*> p <*> getPosition

body :: Parser ([Stx.Stat], Stx.Expr)
body =
  choice
    [ first <$> ((:) <$> (markStat $ letStat <* spaces <* char ';')) <*> (spaces *> body)
    , assembleEvalOrRet <$> (expr <* spaces) <*> optionMaybe (char ';' *> spaces *> body)
    , pure ([], Stx.Unit)
    ]

block :: Parser ([Stx.Stat], Stx.Expr)
block = char '{' *> spaces *> body <* spaces <* char '}'

funcArgs :: Parser Stx.Pat
funcArgs =
  option Stx.PatIgnore $
  notFollowedBy operator *> char '|' *> spaces *> pat <* spaces <* char '|' <* spaces

func :: Parser Stx.Expr
func =
  uncurry <$> (Stx.Func <$> funcArgs) <*> block

atomicExpr :: Parser Stx.Expr
atomicExpr = choice
  [ parenthesized
  , try callNonDot
  , var
  , litUInt
  , litString
  , func
  ]

data Suffix
  = SuffixDotCall (Stx.Path Stx.VarIdent) [Stx.Expr]
  | SuffixCall Stx.Expr

dotCall :: Parser Suffix
dotCall =
  char '.' *> spaces *>
  (uncurry SuffixDotCall <$>
    (first
      <$> ((.) <$> (Stx.Path <$> path) <*> (Stx.DotVarIdent <$> ident))
      <*> (spaces *> callTail)))

suffixCall :: Parser Suffix
suffixCall = SuffixCall <$> slot

suffix :: Parser Suffix
suffix =
  choice
    [ dotCall
    , suffixCall
    ]

markedSuffix :: Parser (Suffix, SourcePos)
markedSuffix = (,) <$> suffix <*> getPosition

applySuffix :: Stx.Expr -> Suffix -> Stx.Expr
applySuffix receiver (SuffixDotCall varIdent args) =
  Stx.Call (Stx.Var varIdent) (foldr1 Stx.Tuple (receiver : args))
applySuffix receiver (SuffixCall arg) =
  Stx.Call receiver arg

applyAndMarkSuffix :: SourcePos -> Stx.Expr -> (Suffix, SourcePos) -> Stx.Expr
applyAndMarkSuffix pos1 receiver (suff, pos2) =
  Stx.Mark pos1 (applySuffix receiver suff) pos2

suffixes :: Parser Stx.Expr
suffixes =
  {-
  Spaces must belong to the *end* of suffixes here, not the beginning, because if spaces are
  consumed at the beginning of a suffix then that suffix will be considered to have consumed input
  and will become non-negotiable.

  In other words, if spaces were considered to belong the beginning of suffixes, then if there were
  trailing spaces after the last suffix, Parsec would treat those spaces as the beginning of another
  (spurious and nonexistent) suffix, try to parse it, and fail.
  -}
  foldl
    <$> (applyAndMarkSuffix <$> getPosition)
    <*> (atomicExpr <* spaces)
    <*> many (markedSuffix <* spaces)

funOp :: Parser Stx.OperatorIdent
funOp =
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

operator :: Parser BinaryOperator
operator =
  choice
    [ FunOp <$> funOp
    , char ',' *> pure TupleOp
    ]

operators :: Parser (UngroupedTerm Stx.Expr BinaryOperator)
operators =
  UngroupedTerm <$> suffixes <*> (spaces *> optionMaybe ((,) <$> operator <*> (spaces *> operators)))

opToExpr :: BinaryOperator -> Stx.Expr -> Stx.Expr -> Stx.Expr
opToExpr (FunOp op) a b = Stx.Call (Stx.Var (Stx.Path [] (Stx.OperatorIdent op))) (Stx.Tuple a b)
opToExpr TupleOp a b = Stx.Tuple a b

groupedOperators :: Parser Stx.Expr
groupedOperators = do
  mayGrouped <- (group deltaGrouping) <$> operators
  case mayGrouped of
    {- TODO:
    This error message exposes implementation details of the compiler by showing the value
    constructor names of operators.  We should print the operators themselves instead (e.g. "+"
    instead of "FunOp OpAdd")
    -}
    Left (op1, op2) -> fail $ "Ambiguous mix of operators " ++ show op1 ++ " and " ++ show op2
    Right grouped -> return $ substitute opToExpr grouped

expr :: Parser Stx.Expr
expr = groupedOperators
