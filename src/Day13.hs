{-# LANGUAGE OverloadedStrings #-}
module Day13 (solveFrom) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Applicative ((<|>))
import Linear.V2
import Data.Array.IArray
import Data.List (find)
import Helpers (readMatrix)

type Pos = V2 Int
type Place = Array Pos Bool

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 values, show $ part2 values)
    where values = map parsePlace . T.splitOn "\n\n" $ txt

parsePlace :: T.Text -> Place
parsePlace = fmap (=='#') . readMatrix

mirColCmp :: Place -> Int -> [Bool]
mirColCmp place n = zipWith (==) colsLeft colsRight
    where colsLeft = [place ! V2 i j |  j <- [n, n-1 .. 1], i <- [1..nRows]]
          colsRight = [place ! V2 i j |  j <- [n+1 .. nCols], i <- [1..nRows]]
          (_, V2 nRows nCols) = bounds place

mirRowCmp :: Place -> Int -> [Bool]
mirRowCmp place n = zipWith (==) rowsAbove rowsBelow
    where rowsAbove = [place ! V2 i j |  i <- [n, n-1 .. 1], j <- [1..nCols]]
          rowsBelow = [place ! V2 i j | i <- [n+1 .. nRows], j <- [1..nCols]]
          (_, V2 nRows nCols) = bounds place

mirrorValue :: Place -> Maybe Int
mirrorValue place = rowIndexV <|> colIndexV
    where (_, V2 nRows nCols) = bounds place 
          rowIndexV = (*100) <$> find (and . mirRowCmp place) [1..nRows - 1]
          colIndexV = find (and . mirColCmp place) [1..nCols - 1]

smudgeValue :: Place -> Maybe Int
smudgeValue place = rowIndexV <|> colIndexV
    where (_, V2 nRows nCols) = bounds place 
          rowIndexV = (*100) <$> find ((==[False]) . filter not . mirRowCmp place) [1..nRows - 1]
          colIndexV = find ((==[False]) . filter not . mirColCmp place) [1..nCols - 1]

part1, part2 :: [Place] -> Maybe Int
part1 = fmap sum . mapM mirrorValue
part2 = fmap sum . mapM smudgeValue
