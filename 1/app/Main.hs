module Main where
import Solver

main :: IO ()
main = do
  inputContent <- readFile "input"
  let parsedInput = parseInput inputContent
  putStrLn $ "Max calories: " ++ show (maxSum $ parsedInput)
  putStrLn $ "Max calories of top three: " ++ show (sumTopThree $ parsedInput)
