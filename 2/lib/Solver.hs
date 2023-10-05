module Solver where

import Data.List.Split

data Shape = Rock | Paper | Scissors deriving (Show, Eq, Enum)
type Round = (Shape, Shape)

parseShape :: Char -> Shape
parseShape 'A' = Rock
parseShape 'B' = Paper
parseShape 'C' = Scissors
parseShape 'X' = Rock
parseShape 'Y' = Paper
parseShape 'Z' = Scissors

splitLine :: String -> (Shape, Shape)
splitLine l = (parseShape $ l!!0, parseShape $ l!!2)

parseInput :: String -> [Round]
parseInput = map splitLine . lines

calculateScoreForRound :: Round -> Int
calculateScoreForRound rs = shapeScore rs + roundScore rs
  where
    shapeScore (_, Rock) = 1
    shapeScore (_, Paper) = 2
    shapeScore (_, Scissors) = 3

    roundScore (Rock, Rock) = 0
    roundScore (Rock, Paper) = 1
    roundScore (Rock, Scissors) = 0
    roundScore (Paper, Rock) = 0
    roundScore (Paper, Paper) = 0
    roundScore (Paper, Scissors) = 1
    roundScore (Scissors, Rock) = 1
    roundScore (Scissors, Paper) = 0
    roundScore (Scissors, Scissors) = 0


calculateScore :: [Round] -> Int
calculateScore = sum . map calculateScoreForRound
