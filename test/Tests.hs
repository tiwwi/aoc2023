module Main (main) where

import System.Exit (exitFailure)

import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

import System.FilePath

import Day01 (solveFrom)
import Day02 (solveFrom)
import Day03 (solveFrom)
import Day04 (solveFrom)
import Day05 (solveFrom)
import Day06 (solveFrom)
import Day07 (solveFrom)
import Day08 (solveFrom)
import Day09 (solveFrom)
import Day10 (solveFrom)
import Day11 (solveFrom)
import Day12 (solveFrom)
import Day13 (solveFrom)
import Day14 (solveFrom)
import Day15 (solveFrom)
import Day16 (solveFrom)
import Day17 (solveFrom)
import Day18 (solveFrom)
import Day19 (solveFrom)
import Day20 (solveFrom)
import Day21 (solveFrom)
import Day22 (solveFrom)
import Day23 (solveFrom)
import Day24 (solveFrom)
import Day25 (solveFrom)

solveDay :: Int -> FilePath -> IO (String, String)
solveDay 1 = Day01.solveFrom
solveDay 2 = Day02.solveFrom
solveDay 3 = Day03.solveFrom
solveDay 4 = Day04.solveFrom
solveDay 5 = Day05.solveFrom
solveDay 6 = Day06.solveFrom
solveDay 7 = Day07.solveFrom
solveDay 8 = Day08.solveFrom
solveDay 9 = Day09.solveFrom
solveDay 10 = Day10.solveFrom
solveDay 11 = Day11.solveFrom
solveDay 12 = Day12.solveFrom
solveDay 13 = Day13.solveFrom
solveDay 14 = Day14.solveFrom
solveDay 15 = Day15.solveFrom
solveDay 16 = Day16.solveFrom
solveDay 17 = Day17.solveFrom
solveDay 18 = Day18.solveFrom
solveDay 19 = Day19.solveFrom
solveDay 20 = Day20.solveFrom
solveDay 21 = Day21.solveFrom
solveDay 22 = Day22.solveFrom
solveDay 23 = Day23.solveFrom
solveDay 24 = Day24.solveFrom
solveDay 25 = Day25.solveFrom
solveDay _ = error "Unknown Day!"

main :: IO ()
main = putStrLn "WHAT" >> defaultMain tests

testedDays :: [Int]
testedDays = [25]
tests :: TestTree
tests = testGroup "All Tests" (testDay <$> testedDays)

testDay :: Int -> TestTree
testDay n = testGroup (printf "day%02d" n) (dayTests n)

dayTests :: Int -> [TestTree]
dayTests 1 = [part1Test 1 "day01_ex1.in" "142", part2Test 1 "day01_ex2.in" "281"]
dayTests 20 = [part1Test 20 "day20_ex11.in" "32000000", part1Test 20 "day20_ex12.in" "11687500"]
dayTests 21 = [part1Test 21 "day21_ex1.in" "16"]
dayTests 22 = [part1Test 22 "day22_ex1.in" "5", part2Test 22 "day22_ex1.in" "7"]
dayTests 23 = [part1Test 23 "day23_ex1.in" "94", part2Test 23 "day23_ex1.in" "154"]
dayTests 24 = [part1Test 24 "day24_ex1.in" "2", part2Test 24 "day24_ex1.in" "47"]
dayTests 25 = [part1Test 25 "day25_ex1.in" "Just 54"]
dayTests _ = error "Unknown Day!"

partTest:: ((String, String) -> String) -> Int -> String -> String -> String -> TestTree
partTest f n name path solution = testCase name runTest
    where
        runTest = do
            calcSol <- f <$> solveDay n ("examples" </> path)
            calcSol @?= solution

part1Test, part2Test :: Int -> String -> String -> TestTree
part1Test n path solution = partTest fst n "part1" path solution
part2Test n path solution = partTest snd n "part2" path solution

