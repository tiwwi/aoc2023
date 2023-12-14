module Day14 (solveFrom) where

import Data.List (intercalate, partition, transpose, findIndex)
import Data.List.Split (splitOn)
import Data.Maybe

data Rock = Round | Cube | Empty deriving (Eq)
type Rocks = [[Rock]]

instance Show Rock where
    show Round = "O"
    show Cube = "#"
    show Empty = "."

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . readFile

solve :: String -> (String, String)
solve txt = (show $ part1 values, show $ part2 values)
    where values = parseRocks txt

parseRocks :: String -> Rocks
parseRocks = transpose . fmap (map toRocks) . lines
    where toRocks 'O' = Round
          toRocks '#' = Cube
          toRocks '.' = Empty
          toRocks _ = error "Not a rock!"

shiftColumn :: [Rock] -> [Rock]
shiftColumn rocks = intercalate [Cube] postFix
    where postFix = moveRounds <$> splitOn [Cube] rocks
          moveRounds = uncurry (++) . partition (==Round)

columnScore :: [Rock] -> Int
columnScore rocks = sum $ map fst $ filter ((==Round) . snd) $ zip [n, n-1 ..] rocks
    where n = length rocks

rotateClockwise :: Rocks -> Rocks
rotateClockwise = transpose . map reverse

shiftNorth :: Rocks -> Rocks
shiftNorth = map shiftColumn

secondInf :: [a] -> [a]
secondInf xs = take 1 xs ++ (secondInf $ drop 2 xs)

rockScore, part1, part2 :: Rocks -> Int
rockScore = sum . map columnScore
part1 = rockScore . shiftNorth
part2 rocks = rockScore $ remaining !! ((1000000000 - cycleStart) `mod` cycleLength)
    where cycleLength = 1 + (fromJust $ findIndex (== head remaining) (tail remaining))
          cycleStep = foldr1 (.) $ replicate 4 (rotateClockwise . shiftNorth)
          (tort, hare) = (iterate cycleStep rocks, secondInf tort)
          remaining = drop cycleStart tort
          cycleStart = 1 + (fromJust $ findIndex (uncurry (==)) $ tail $ zip tort hare)
