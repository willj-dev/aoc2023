{-# LANGUAGE InstanceSigs #-}
module Day03 where

import Parsers
import qualified InputData as Data

import Data.Text (Text)
import Text.Megaparsec (parse, eof)
import Text.Megaparsec.Char (printChar, newline)
import Control.Applicative.Combinators (sepEndBy, some)
import Data.Maybe (maybeToList, mapMaybe)
import Data.Char (isDigit, digitToInt, isSymbol, isPunctuation)

type Coordinate = (Int, Int)
data PartNumber = PartNumber { value :: Int, start :: Coordinate, pnLength :: Int } deriving (Eq, Show)
data Schematic = Schematic { symbols :: [Coordinate], partNumbers :: [PartNumber] } deriving (Show)

instance Semigroup Schematic where
  (<>) :: Schematic -> Schematic -> Schematic
  (Schematic ss ps) <> (Schematic ss' ps') = Schematic (ss ++ ss') (ps ++ ps')

instance Monoid Schematic where
  mempty :: Schematic
  mempty = Schematic [] []

parseSchematic :: Text -> Either ParseError Schematic
parseSchematic input = readSchematic (1, 1) Nothing <$> parse charLinesParser "day 03" input
  where
    charLinesParser = (some printChar `sepEndBy` newline) <* eof

    -- end of schematic
    readSchematic (_, _) _ [] = mempty

    -- end of line; increment y, finish part number if one is in progress
    readSchematic (_, y) pn ("":ls) = Schematic [] (maybeToList pn) <> readSchematic (1, y + 1) Nothing ls

    -- parse a schematic character
    readSchematic (x, y) pn ((c:cs):ls)
      -- period is a blank; skip it, possibly ending a running part number
      | c == '.' = Schematic [] (maybeToList pn) <> readSchematic (x + 1, y) Nothing (cs:ls)

      -- start a new part number, or add a digit to a running one
      | isDigit c = let
        pn' = Just $ case pn of
          Nothing -> PartNumber (digitToInt c) (x, y) 1
          Just (PartNumber v s l) -> PartNumber (10*v + digitToInt c) s (l + 1)
        in readSchematic (x + 1, y) pn' (cs:ls)

      -- add a part coordinate, possibly ending a running part number
      | isSymbol c || isPunctuation c = Schematic [(x, y)] (maybeToList pn) <> readSchematic (x + 1, y) Nothing (cs:ls)

      -- ???
      | otherwise = error $ "readSchematic: unexpected character " ++ [c]

-- returns a list of coordinates adjacent to a given coordinate, including diagonally
adjacentCoordinates :: Coordinate -> [Coordinate]
adjacentCoordinates (x, y) =
  [
    (x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
    (x - 1, y),                 (x + 1, y),
    (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
  ]

-- checks if any of a part number's digits are at some coordinate
containsCoordinate :: PartNumber -> Coordinate -> Bool
containsCoordinate (PartNumber _ (x, y) n) (x', y') = sameLine && dx >= 0 && dx < n
  where
    sameLine = y == y'
    dx = x' - x

-- checks if a given part number is adjacent to any symbol
isValidPartNumber :: PartNumber -> [Coordinate] -> Bool
isValidPartNumber pn = any (any (containsCoordinate pn) . adjacentCoordinates)

-- add up all the part numbers in a schematic
sumPartNumbers :: Schematic -> Int
sumPartNumbers (Schematic ss pns) = sum (value <$> filter (`isValidPartNumber` ss) pns)

-- filter for part numbers that are adjacent to the given coordinate
adjacentPartNumbers :: Coordinate -> [PartNumber] -> [PartNumber]
adjacentPartNumbers c = filter (\pn -> any (containsCoordinate pn) (adjacentCoordinates c))

-- if a component has exactly two adjacent part numbers, it is a gear whose gear ratio is the product of those numbers
gearRatio :: Coordinate -> [PartNumber] -> Maybe Int
gearRatio c pns = let
  pns' = adjacentPartNumbers c pns
  in if length pns' == 2
    then let
      pn1 = value $ head pns'
      pn2 = value . head $ tail pns'
      in Just $ pn1 * pn2
    else Nothing

-- add up all the gear ratios in a schematic
sumGearRatios :: Schematic -> Int
sumGearRatios (Schematic ss pns) = sum (mapMaybe (`gearRatio` pns) ss)

-- what's the sum of all the part numbers?
day03 :: Text -> Either ParseError Int
day03 input = sumPartNumbers <$> parseSchematic input

-- what's the sum of all the gear ratios?
day03' :: Text -> Either ParseError Int
day03' input = sumGearRatios <$> parseSchematic input

runDay03 :: IO ()
runDay03 = do
  textData <- Data.day 3
  prettyParseResult (day03' textData)
