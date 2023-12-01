module Main where

import Control.Monad ((>=>))
import Day01 qualified (solveFrom)
import System.Environment (getArgs)
import Text.Printf (printf)

inputFile :: Int -> FilePath
inputFile n = "inputs/day" ++ printf "%02d" n ++ ".in"

solveDay :: Int -> IO (String, String)
solveDay 1 = Day01.solveFrom $ inputFile 1
solveDay _ = undefined

displaySolution :: (String, String) -> String
displaySolution (x, y) = printf "Part 1: %s\nPart 2: %s" x y

runDays :: [Int] -> IO ()
runDays xs = mapM_ (fmap displaySolution . solveDay >=> putStrLn) $ fill xs
  where
    fill [] = [1 .. 25]
    fill xsp = xsp

main :: IO ()
main = getArgs >>= runDays . map read
