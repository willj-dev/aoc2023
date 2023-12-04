{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Day04 where

import Parsers
import qualified InputData as Data

import Data.Text (Text)
import Text.Megaparsec (parse, eof)
import Text.Megaparsec.Char (eol, string, digitChar, char)
import Control.Applicative.Combinators (sepEndBy, some)
import Data.Foldable (foldl')
import Data.Array (Array, elems, accum, listArray, (!))

data Scratchy = Scratchy { cardId :: Int, winningNumbers :: [Int], cardNumbers :: [Int] } deriving (Show)

parseScratchies :: Text -> Either ParseError [Scratchy]
parseScratchies = parse (scratchiesParser <* eof) "day 4"
  where
    scratchiesParser = scratchyParser `sepEndBy` eol

    spaceSeparatedNums :: Parser [Int]
    spaceSeparatedNums = fmap read <$> (some digitChar `sepEndBy` spaces)

    scratchyParser = do
      string "Card"
      spaces
      cid <- read <$> some digitChar
      char ':'
      spaces
      wns <- spaceSeparatedNums
      char '|'
      spaces
      Scratchy cid wns <$> spaceSeparatedNums

points :: Scratchy -> Int
points (Scratchy _ wns cns) = foldl' addPoints 0 cns
  where
    addPoints score num
      | num `elem` wns  = if score == 0 then 1 else score * 2
      | otherwise       = score

copies :: Scratchy -> Int
copies (Scratchy _ wns cns) = length (filter (`elem` wns) cns)

countCopies :: [Scratchy] -> Int
countCopies cs = sum . elems . accumCopies oneOfEach $ cs
  where
    newCopies :: Int -> Scratchy -> [(Int, Int)]
    newCopies mult c@(Scratchy cid _ _) = let n = copies c in [(cid + i, mult) | i <- [ 1 .. n ], i <= snd scratchyArrayBounds]

    scratchyArrayBounds = (1, length cs)
    oneOfEach = listArray scratchyArrayBounds (repeat 1)

    accumCopies :: Array Int Int -> [Scratchy] -> Array Int Int
    accumCopies counts [] = counts
    accumCopies counts (c@(Scratchy cid _ _):cs') = accumCopies (accum (+) counts (newCopies (counts ! cid) c)) cs'

day04 :: Text -> Either ParseError Int
day04 input = sum . fmap points <$> parseScratchies input

day04' :: Text -> Either ParseError Int
day04' input = countCopies <$> parseScratchies input

runDay04 :: IO ()
runDay04 = do
  textData <- Data.day 4
  prettyParseResult (day04' textData)
