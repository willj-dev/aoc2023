module Day01 where

import Parsers ( ParseError, parseNumLines )
import qualified InputData as Data

import Control.Applicative (liftA2)
import Control.Monad ((>=>))
import Data.Text (Text)
import Text.Megaparsec (parse, eof)

parseEntries :: Text -> Either ParseError [Int]
parseEntries = parse (parseNumLines <* eof) "day 01"

entryCombinations :: Int -> [Int] -> Maybe [[Int]]
entryCombinations 0 _ = Just []
entryCombinations 1 xs = Just [[x] | x <- xs]
entryCombinations _ [] = Nothing
entryCombinations n xs
    | n < 0 = Nothing
    | n > length xs = Nothing
    | n == length xs = Just [xs]
entryCombinations n (x:xs) = liftA2 (++) these those where
    these = (\cs -> (x:) <$> cs) <$> entryCombinations (n - 1) xs
    those = entryCombinations n xs

findCombinationWithSum :: Int -> [[Int]] -> Maybe [Int]
findCombinationWithSum _ [] = Nothing
findCombinationWithSum s ( c : cs ) = if sum c == s then Just c else findCombinationWithSum s cs

day01 :: Text -> Either ParseError (Maybe [Int])
day01 input = (entryCombinations 2 >=> findCombinationWithSum 2020) <$> parseEntries input

day01' :: Text -> Either ParseError (Maybe [Int])
day01' input = (entryCombinations 3 >=> findCombinationWithSum 2020) <$> parseEntries input

runDay01 :: IO ()
runDay01 = do
    textData <- Data.day 1
    print $ day01' textData

