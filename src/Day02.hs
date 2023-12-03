{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Day02 where

import Parsers (ParseError, spaces)
import qualified InputData as Data

import Control.Applicative.Combinators (sepEndBy, some, sepBy)
import Data.Foldable (find)
import Data.Text (Text)
import Text.Megaparsec (MonadParsec(eof), parse, errorBundlePretty)
import Text.Megaparsec.Char (eol, string, char, digitChar, space, letterChar)

type Game = [Handful]
data CubeColor = Red | Green | Blue deriving (Eq, Show)
type Handful = [(CubeColor, Int)]

colorNameToCubeColor :: String -> CubeColor
colorNameToCubeColor "red"    = Red
colorNameToCubeColor "green"  = Green
colorNameToCubeColor "blue"   = Blue
colorNameToCubeColor name     = error $ "colorNameToCubeColor: unexpected color name " ++ name

parseRecords :: Text -> Either ParseError [(Int, Game)]
parseRecords = parse (recordParser <* eof) "day 02"
  where
    recordParser = gameParser `sepEndBy` eol

    gameParser = do
      string "Game "
      gameNumber <- read <$> some digitChar
      char ':'
      handfuls <- (spaces *> handfulParser <* spaces) `sepBy` char ';'
      return (gameNumber, handfuls)

    handfulParser = (spaces *> colorCountParser <* spaces) `sepBy` char ','
    colorCountParser = do
      num <- read <$> some digitChar
      space
      color <- colorNameToCubeColor <$> some letterChar
      return (color, num)

countValidGames :: Int -> Int -> Int -> [(Int, Game)] -> Int
countValidGames _ _ _ [] = 0
countValidGames r g b ((gameId, game):games) = result + countValidGames r g b games
  where
    result = if isValidGame then gameId else 0

    isValidGame = all (all isValidColorCount) game

    isValidColorCount (Red, c)    = c <= r
    isValidColorCount (Green, c)  = c <= g
    isValidColorCount (Blue, c)   = c <= b

sumMinimumGamePowers :: [(Int, Game)] -> Int
sumMinimumGamePowers = sum . fmap (minimumGamePower 0 0 0 . snd)
  where
    minimumGamePower r g b [] = r * g * b
    minimumGamePower r g b (h:hs) = let
      r' = maybe r (\(_, count) -> max r count) $ find (\(color, _) -> color == Red) h
      g' = maybe g (\(_, count) -> max g count) $ find (\(color, _) -> color == Green) h
      b' = maybe b (\(_, count) -> max b count) $ find (\(color, _) -> color == Blue) h
      in minimumGamePower r' g' b' hs

day02 :: Text -> Either ParseError Int
day02 input = countValidGames 12 13 14 <$> parseRecords input

day02' :: Text -> Either ParseError Int
day02' input = sumMinimumGamePowers <$> parseRecords input

runDay02 :: IO ()
runDay02 = do
  textData <- Data.day 2
  case day02' textData of
    Left e -> putStrLn $ errorBundlePretty e
    Right c -> print c
