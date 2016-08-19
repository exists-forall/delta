module ParseUtils
  ( module Text.Parsec
  , Parser
  , both
  , rangeParser
  )
where

import Text.Parsec

import Data.Text.Lazy (Text)

type Parser = Parsec Text ()

both :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
both f g x = f x && g x

rangeParser :: (Enum a) => String -> Char -> Char -> Parser a
rangeParser l a b =
  flip label l $
  fmap (toEnum . (subtract (fromEnum a)) . fromEnum) $
  satisfy (both (>= a) (<= b))

-- Mostly for testing
fullParse :: Parser a -> Text -> Either ParseError a
fullParse p = parse (p <* eof) ""
