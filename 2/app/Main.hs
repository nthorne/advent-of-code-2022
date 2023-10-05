module Main where

import Solver

main :: IO ()
main = do
  inputContent <- readFile "input"
  let parsedInput = parseInput inputContent
  putStrLn $ "The score is: " ++ show (calculateScore parsedInput)
