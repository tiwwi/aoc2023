module Day09 (solveFrom) where

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . readFile

solve :: String -> (String, String)
solve txt = (show $ part1 parsed, show $ part2 parsed)
    where parsed = reverse . map read . words <$> lines txt :: [[Int]]

step :: [Int] -> [Int]
step xs = zipWith (-) xs (tail xs)

differences :: [Int] -> [[Int]]
differences = takeWhile (any (/=0)) . iterate step

part1Transform, part2Transform :: [Int] -> Int
part1Transform = sum . map head . differences
part2Transform = foldr1 (-) . map last . differences

part1, part2 :: [[Int]] -> Int
part1 = sum . map part1Transform
part2 = sum . map part2Transform
