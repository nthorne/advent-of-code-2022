module Main where

import Data.Char (isLower, isUpper, ord, isSpace)
import Data.List (foldr1, intersect, nub)
import Data.List.Split (chunksOf)

computeItemPriority :: Char -> Int
computeItemPriority c
  | isLower c = (ord c) - (ord 'a') + 1
  | isUpper c = (ord c) - (ord 'A') + 27
  | otherwise = 0

parse :: [String] -> [String]
parse = map (nub . foldr1 intersect) . chunksOf 3

solve :: [String] -> Int
solve = sum . map computeItemPriority . concat

main :: IO ()
main = readFile "input" >>= print . solve . parse . lines
