module Solver where
import Text.Read (readMaybe)
import Data.List.Split
import Data.Maybe (catMaybes)
import Data.List (sort)

-- Given a list of list of integers, return the max sum of all those lists
-- where the sum is the sum of the integers in the list.
maxSum :: [[Int]] -> Int
maxSum = maximum . map sum

sumTopThree :: [[Int]] -> Int
sumTopThree = sum . take 3 . reverse . sort . map sum

-- Assuming that the input string is a list of integers, we split that string
-- into a list of lists of integers, where the list delimiter is a newline.
parseInput :: String -> [[Int]]
parseInput l = mapToInt . splitOnNothings . map readToMaybe $ lines l
  where
    readToMaybe e = readMaybe e :: Maybe Int
    splitOnNothings = split (dropDelims $ dropBlanks $ whenElt (Nothing==))
    mapToInt = map catMaybes
