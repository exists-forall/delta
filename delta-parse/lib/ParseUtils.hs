{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ParseUtils
  ( module Text.Parsec
  , SourcePos (..)
  , fromParsecSourcePos
  , getPosition
  , Parser
  , both
  , rangeParser
  , spaces
  , sepByTrailing
  , semicolonDelimited
  , fullParse
  )
where

import GHC.Generics (Generic)

import Text.Parsec hiding (space, spaces, getPosition, SourcePos)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Char as ParsecChar (space)
import Data.Aeson.Types (FromJSON, ToJSON)

import Data.Text.Lazy (Text)

data SourcePos
  = SourcePos
    { line :: Int
    , col :: Int
    }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

fromParsecSourcePos :: Parsec.SourcePos -> SourcePos
fromParsecSourcePos pos = SourcePos (Parsec.sourceLine pos) (Parsec.sourceColumn pos)

getPosition :: Parser SourcePos
getPosition = fromParsecSourcePos <$> Parsec.getPosition

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
