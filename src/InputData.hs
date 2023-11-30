module InputData where

import Prelude hiding (readFile)

import Paths_aoc2023 (getDataFileName)
import Data.Text (Text)
import Data.Text.IO (readFile)
import Text.Printf (printf)

getDataFile :: String -> IO Text
getDataFile name = do
    fileName <- getDataFileName ("data/" ++ name)
    readFile fileName

day :: Int -> IO Text
day n = getDataFile dataFileName where
    dataFileName = printf "Day%02d.txt" n
