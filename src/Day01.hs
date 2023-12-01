module Day01 (
    firstLastDigit,
    firstLastDigit',
    twoDigits,
    day01,
    day01',
    runDay01
) where

import Parsers ( ParseError, parseStringLines )
import qualified InputData as Data

import Data.Foldable (foldl')
import Data.Text (Text)
import Text.Megaparsec (parse, eof)
import Data.Char (isDigit, digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

parseEntries :: Text -> Either ParseError [String]
parseEntries = parse (parseStringLines <* eof) "day 01"

stringToNumber :: String -> Int
stringToNumber = twoDigits . firstLastDigit

stringToNumber' :: String -> Int
stringToNumber' = twoDigits . firstLastDigit'

firstLastDigit :: String -> (Int, Int)
firstLastDigit "" = error "firstLastDigit: empty string"
firstLastDigit cs = foldl' go (0, 0) cs
    where
        -- looks at each character, remembering the first and last seen digits
        go (f, l) c =
            if isDigit c
            then let
                d = digitToInt c

                 -- if we haven't seen one yet, this is the first. Otherwise, keep the original.
                f' = if f > 0 then f else d
                in (f', d)
            else (f, l)

firstLastDigit' :: String -> (Int, Int)
firstLastDigit' = fld 0 0
    where
        -- looks at each character, remembering the first and last seen digits
        fld f l "" = (f, l)
        fld f l ss@(c:cs) =
            if isDigit c then let
                d = digitToInt c
                in fld (keepFirst f d) d cs
            else case findFirstDigitSubstring ss of
                Nothing -> fld f l cs
                Just d -> fld (keepFirst f d) d cs

        keepFirst f f'
            | f > 0     = f
            | otherwise = f'

digitNames :: [String]
digitNames = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

isDigitString :: String -> Bool
isDigitString s = s `elem` digitNames

findFirstDigitSubstring :: String -> Maybe Int
findFirstDigitSubstring s = ffds 3 where
    maxLen = length s
    ffds n = if n > maxLen
        then Nothing
        else let ss = take n s in
            if isDigitString ss
                then Just $ digitValue ss
                else ffds (n + 1)

digitValue :: String -> Int
digitValue s = 1 + fromMaybe (error $ "digitValue: invalid digit name " ++ s) (elemIndex s digitNames)

twoDigits :: (Int, Int) -> Int
twoDigits (t, o) = 10 * t + o

day01 :: Text -> Either ParseError Int
day01 input = sum . fmap stringToNumber <$> parseEntries input

day01' :: Text -> Either ParseError Int
day01' input = sum . fmap stringToNumber' <$> parseEntries input

runDay01 :: IO ()
runDay01 = do
    textData <- Data.day 1
    print $ day01 textData
    print $ day01' textData
