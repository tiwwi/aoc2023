module Main where

import Control.Monad ((>=>))
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Time.Clock

import qualified Day01 (solveFrom)
import qualified Day02 (solveFrom)
import qualified Day03 (solveFrom)
import qualified Day04 (solveFrom)
import qualified Day05 (solveFrom)
import qualified Day06 (solveFrom)
import qualified Day07 (solveFrom)
import qualified Day08 (solveFrom)
import qualified Day09 (solveFrom)
import qualified Day10 (solveFrom)
import qualified Day11 (solveFrom)
import qualified Day12 (solveFrom)
import qualified Day13 (solveFrom)
import qualified Day14 (solveFrom)
import qualified Day15 (solveFrom)
import qualified Day16 (solveFrom)
import qualified Day17 (solveFrom)
import qualified Day18 (solveFrom)
import qualified Day19 (solveFrom)
import qualified Day20 (solveFrom)
import qualified Day21 (solveFrom)
import qualified Day22 (solveFrom)
import qualified Day23 (solveFrom)
import qualified Day24 (solveFrom)
import qualified Day25 (solveFrom)

inputFile :: Int -> FilePath
inputFile n = "inputs/day" ++ printf "%02d" n ++ ".in"

solveDay :: Int -> IO (String, String)
solveDay 1 = Day01.solveFrom $ inputFile 1
solveDay 2 = Day02.solveFrom $ inputFile 2
solveDay 3 = Day03.solveFrom $ inputFile 3
solveDay 4 = Day04.solveFrom $ inputFile 4
solveDay 5 = Day05.solveFrom $ inputFile 5
solveDay 6 = Day06.solveFrom $ inputFile 6
solveDay 7 = Day07.solveFrom $ inputFile 7
solveDay 8 = Day08.solveFrom $ inputFile 8
solveDay 9 = Day09.solveFrom $ inputFile 9
solveDay 10 = Day10.solveFrom $ inputFile 10
solveDay 11 = Day11.solveFrom $ inputFile 11
solveDay 12 = Day12.solveFrom $ inputFile 12
solveDay 13 = Day13.solveFrom $ inputFile 13
solveDay 14 = Day14.solveFrom $ inputFile 14
solveDay 15 = Day15.solveFrom $ inputFile 15
solveDay 16 = Day16.solveFrom $ inputFile 16
solveDay 17 = Day17.solveFrom $ inputFile 17
solveDay 18 = Day18.solveFrom $ inputFile 18
solveDay 19 = Day19.solveFrom $ inputFile 19
solveDay 20 = Day20.solveFrom $ inputFile 20
solveDay 21 = Day21.solveFrom $ inputFile 21
solveDay 22 = Day22.solveFrom $ inputFile 22
solveDay 23 = Day23.solveFrom $ inputFile 23
solveDay 24 = Day24.solveFrom $ inputFile 24
solveDay 25 = Day25.solveFrom $ inputFile 25
solveDay _ = error "Unknown Day!"


displayDay :: Int -> NominalDiffTime -> (String, String) -> String
displayDay n time (x, y) = printf "=== Day %02d ===\nPart 1: %s\nPart 2: %s\nTime: %s\n" n x y (show time)

runDay :: Int -> IO String
runDay n = do
    start <- getCurrentTime
    result <- solveDay n
    end <- getCurrentTime
    let timeDelta = diffUTCTime end start
    return $ displayDay n timeDelta result

runDays :: [Int] -> IO ()
runDays xs = mapM_ (runDay >=> putStrLn) $ fill xs
  where
    fill [] = [1 .. 25]
    fill xsp = xsp

main :: IO ()
main = getArgs >>= runDays . map read
