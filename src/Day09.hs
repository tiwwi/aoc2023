module Day09 (solveFrom) where

import Data.List.HT (takeUntil)

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . readFile

solve :: String -> (String, String)
solve txt = (show $ part1 parsed, show $ part2 parsed)
    where parsed = reverse . map read . words <$> lines txt :: [[Int]]

step :: [Int] -> [Int]
step xs = zipWith (-) xs (tail xs)

differences :: [Int] -> [[Int]]
differences = takeUntil (all (==0)) . iterate step

part1Transform, part2Transform :: [Int] -> [Int]
part1Transform = scanr1 (+) . map head . differences
part2Transform = scanr1 (-) . map last . differences

part1, part2 :: [[Int]] -> Int
part1 = sum . map (head . part1Transform)
part2 = sum . map (head . part2Transform)
