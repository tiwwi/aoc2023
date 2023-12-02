{-# LANGUAGE OverloadedStrings #-}

module Day02 (solveFrom) where

import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T

data Pull = Pull {red :: Int, green :: Int, blue :: Int} deriving (Show)
data Game = Game {idx :: Int, pulls :: [Pull]} deriving (Show)

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve s = (show $ part1 ps, show $ part2 ps)
  where
    ps = parseInput s

splitOnOnce :: T.Text -> T.Text -> (T.Text, T.Text)
splitOnOnce txt = second (T.drop $ T.length txt) . T.breakOn txt

parseInput :: T.Text -> [Game]
parseInput = map parseGame . T.lines

parseGame :: T.Text -> Game
parseGame txt = Game idx (parsePull <$> T.splitOn "; " values)
  where
    (gameId, values) = splitOnOnce ": " txt
    idx = read $ T.unpack $ snd $ splitOnOnce " " gameId

parsePull :: T.Text -> Pull
parsePull txt = Pull red green blue
  where
    each = (\(s1, s2) -> (s2, read $ T.unpack s1)) . splitOnOnce " " <$> T.splitOn ", " txt
    red = fromMaybe 0 $ lookup "red" each
    green = fromMaybe 0 $ lookup "green" each
    blue = fromMaybe 0 $ lookup "blue" each

isValid :: Game -> Bool
isValid = all isGood . pulls
  where
    isGood (Pull r g b) = r <= 12 && g <= 13 && b <= 14

powerScore :: Game -> Int
powerScore game = product $ maximum . (<$> pulls game) <$> [red, green, blue]

part1 :: [Game] -> Int
part1 = sum . map idx . filter isValid

part2 :: [Game] -> Int
part2 = sum . map powerScore
