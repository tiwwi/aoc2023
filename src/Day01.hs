module Day01 (solveFrom) where

import Control.Monad (msum)
import Data.Bifunctor (first)
import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe (fromJust)

type Input = [String]

numWith :: [(String, Int)]
numWith = zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1 ..]

solveFrom :: FilePath -> IO (String, String)
solveFrom path = solve <$> readFile path

solve :: String -> (String, String)
solve s = (show $ part1 ps, show $ part2 ps)
  where
    ps = parseInput s

parseInput :: String -> Input
parseInput = lines

lookupFn :: (a -> Bool) -> [(a, b)] -> Maybe b
lookupFn f xs = lookup True $ map (first f) xs

getNum2 :: String -> Maybe Int
getNum2 [] = Nothing
getNum2 xs@(x : _)
  | isDigit x = Just $ digitToInt x
  | otherwise = lookupFn (`isPrefixOf` xs) numWith

getNum1 :: String -> Maybe Int
getNum1 [] = Nothing
getNum1 (x : _)
  | isDigit x = Just $ digitToInt x
  | otherwise = Nothing

calValue :: (String -> Maybe Int) -> String -> Maybe Int
calValue f s = (+) <$> ((10 *) <$> firstNr) <*> secondNr
  where
    firstNr = msum $ f <$> tails s
    secondNr = msum $ f <$> reverse (tails s)

part1 :: Input -> Int
part1 = sum . fromJust . mapM (calValue getNum1)

part2 :: Input -> Int
part2 = sum . fromJust . mapM (calValue getNum2)
