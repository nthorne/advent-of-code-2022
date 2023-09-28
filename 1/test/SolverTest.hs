module Main where
import Test.HUnit
import Solver
import qualified System.Exit as Exit

testParseInput :: Test
testParseInput = TestCase (assertEqual "Basic scenario" [[1,2,3],[4,5,6],[7,8,9]] (parseInput "1\n2\n3\n\n4\n5\n6\n\n7\n8\n9"))

tests :: Test
tests = TestList [TestLabel "Test parsing input to a list of a list of Ints" testParseInput]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
