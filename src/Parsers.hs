module Parsers where

import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec (Parsec, ParseErrorBundle, errorBundlePretty)
import Text.Megaparsec.Char (eol, char, alphaNumChar)
import Text.Megaparsec.Char.Lexer (decimal, signed)

import Control.Applicative.Combinators (sepEndBy, sepBy, some, many)

type ParseError = ParseErrorBundle Text Void
type Parser = Parsec Void Text

signedInt :: Parser Int
signedInt = signed (return ()) decimal

parseNumLines :: Parser [Int]
parseNumLines = signedInt `sepEndBy` eol

parseCsvNums :: Parser [Int]
parseCsvNums = signedInt `sepBy` char ','

parseStringLines :: Parser [String]
parseStringLines = some alphaNumChar `sepEndBy` eol

spaces :: Parser String
spaces = many (char ' ')

prettyParseResult :: (Show a) => Either ParseError a -> IO ()
prettyParseResult (Left e) = putStrLn $ errorBundlePretty e
prettyParseResult (Right r) = print r
