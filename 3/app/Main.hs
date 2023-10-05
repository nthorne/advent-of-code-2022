module Main where

import Data.Char (isLower, isUpper, ord, isSpace)
import Data.List (nub)

-- For now, we assume that the input is well formed.
splitInHalf :: [Char] -> ([Char], [Char])
splitInHalf xs = splitAt (length xs `div` 2) xs

computeItemPriority :: Char -> Int
computeItemPriority c
  | isLower c = (ord c) - (ord 'a') + 1
  | isUpper c = (ord c) - (ord 'A') + 27
  | otherwise = 0

-- Find items that are duplicated between the two compartments,
-- note that an item might be duplicated more than once.
findDuplicates :: ([Char], [Char]) -> [Char]
findDuplicates ([], _) = []
findDuplicates (_, []) = []
findDuplicates ((x:xs), ys)
  | elem x ys = x : findDuplicates (xs, ys)
  | otherwise = findDuplicates (xs, ys)

calculateRucksackPriorities :: [Char] -> [Int]
calculateRucksackPriorities = concat . priorities . uniqueDuplicates . compartmentalized
  where
    compartmentalized = map splitInHalf . lines
    uniqueDuplicates = map (nub . findDuplicates)
    priorities = map (map computeItemPriority)

main :: IO ()
main = readFile "input" >>= print . sum . calculateRucksackPriorities
