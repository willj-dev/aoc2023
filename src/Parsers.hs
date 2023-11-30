module Parsers where

import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec (Parsec, ParseErrorBundle)
import Text.Megaparsec.Char (eol, char)
import Text.Megaparsec.Char.Lexer (decimal, signed)

import Control.Monad.Combinators (sepEndBy, sepBy)

type ParseError = ParseErrorBundle Text Void
type Parser = Parsec Void Text

signedInt :: Parser Int
signedInt = signed (return ()) decimal

parseNumLines :: Parser [Int]
parseNumLines = signedInt `sepEndBy` eol

parseCsvNums :: Parser [Int]
parseCsvNums = signedInt `sepBy` char ','