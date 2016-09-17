module ParseExpr
  ( expr
  , body
  )
where

import ParseUtils

import Data.Text (Text)

import qualified Syntax as Stx
import ParseIdent
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
    , litSeq
    , func
    ]

qualifiedCall :: Parser (Stx.VarIdent, [Stx.Expr]) -> Parser (Stx.Path Stx.VarIdent, [Stx.Expr])
qualifiedCall p = first <$> (Stx.Path <$> path) <*> p

callNonDot :: Parser Stx.Expr
callNonDot =
  mark $ assemble <$> qualifiedCall (varIdentNonDotWithSlot' ForbidReserved slot) where
  assemble (varName, args) = Stx.Call (Stx.Var varName) (foldr1 Stx.Tuple args)

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

litSeq :: Parser Stx.Expr
litSeq = mark $
  Stx.LitSeq <$> (char '[' *> spaces *> semicolonDelimited expr <* spaces <* char ']')

letBinding :: Parser (Stx.Pat, Stx.Expr)
letBinding =
  (,) <$> try (pat <* spaces <* notFollowedBy operator <* char '=') <*> (spaces *> expr)

assembleEvalOrRet :: Stx.Expr -> Maybe Stx.Expr -> Stx.Expr
assembleEvalOrRet e (Just ret) = Stx.Let (Stx.PatIgnore Nothing) e ret
assembleEvalOrRet e Nothing = e

body :: Parser Stx.Expr
body =
  choice
    [ uncurry Stx.Let <$> (letBinding <* spaces <* char ';') <*> (spaces *> body)
    , assembleEvalOrRet <$> (expr <* spaces) <*> optionMaybe (char ';' *> spaces *> body)
    , pure Stx.Unit
    ]

block :: Parser Stx.Expr
block =
  choice
    [ char '{' *> spaces *> body <* spaces <* char '}'
    , keyword "do" *> spaces *> body <* spaces
    ]

funcArgs :: Parser Stx.Pat
funcArgs =
  option (Stx.PatIgnore Nothing) $
  notFollowedBy operator *> char '|' *> spaces *> pat <* spaces <* char '|' <* spaces

func :: Parser Stx.Expr
func =
  Stx.Func <$> funcArgs <*> block

atomicExpr :: Parser Stx.Expr
atomicExpr = choice
  [ parenthesized
  , try callNonDot
  , var
  , litUInt
  , litString
  , litSeq
  , func
  ]

data Suffix
  = SuffixDotCall (Stx.Path Stx.VarIdent) [Stx.Expr]
  | SuffixCall Stx.Expr

dotCall :: Parser Suffix
dotCall =
  char '.' *> spaces *>
    (uncurry SuffixDotCall <$> qualifiedCall (varIdentDotSuffixWithSlot' ForbidReserved slot))

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

operator :: Parser BinaryOperator
operator =
  choice
    [ FunOp <$> operatorIdent
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
