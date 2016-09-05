module ParseUtils
  ( module Text.Parsec
  , Parser
  , both
  , rangeParser
  , spaces
  , sepByTrailing
  , semicolonDelimited
  , fullParse
  )
where

import Text.Parsec hiding (space, spaces)
import qualified Text.Parsec.Char as ParsecChar (space)

import Data.Text.Lazy (Text)

type Parser = Parsec Text ()

both :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
both f g x = f x && g x

rangeParser :: (Enum a) => String -> Char -> Char -> Parser a
rangeParser l a b =
  flip label l $
  fmap (toEnum . (subtract (fromEnum a)) . fromEnum) $
  satisfy (both (>= a) (<= b))

lineComment :: Parser ()
lineComment = try (string "//") *> skipMany (noneOf "\r\n")

spaces :: Parser ()
spaces = skipMany ((ParsecChar.space *> pure ()) <|> lineComment)

sepByTrailing :: Parser a -> Parser sep -> Parser [a]
sepByTrailing item sep =
  option [] ((:) <$> item <*> option [] (sep *> sepByTrailing item sep))

semicolonDelimited :: Parser a -> Parser [a]
semicolonDelimited p = sepByTrailing (p <* spaces) (char ';' *> spaces)

-- Mostly for testing
fullParse :: Parser a -> Text -> Either ParseError a
fullParse p = parse (p <* eof) ""
