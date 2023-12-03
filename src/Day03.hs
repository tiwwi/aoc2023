module Day03 (solveFrom) where

import Data.Array.IArray
import Data.Char (digitToInt, isDigit)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace
import Helpers (decimalToInt)

data Field = Symbol Char | Number Int | Empty deriving (Show, Eq)
data NumPos = NumPos Int Int Int deriving(Show)

type Pos = (Int, Int)
type Schematic = Array Pos Field

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 schema, show $ part2 schema)
    where schema = parseSchematic txt


charToField :: Char -> Field
charToField '.' = Empty
charToField c
  | isDigit c = Number $ digitToInt c
  | otherwise = Symbol c

parseSchematic :: T.Text -> Schematic
parseSchematic txt = listArray bds $ (lns >>= map charToField . T.unpack)
  where
    lns = T.lines txt
    nCols = T.length $ head lns
    nRows = length lns
    bds = ((1, 1), (nRows, nCols))

testParse :: IO ()
testParse = do
  txt <- T.readFile "inputs/ex03.in"
  print txt
  print $ getNumbers $ parseSchematic txt

isNumber :: Field -> Bool
isNumber (Number _) = True
isNumber _ = False

isSymbol :: Field -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

getNumbers :: Schematic -> [NumPos]
getNumbers schema = foldr go [] $ [ix | (ix, field) <- assocs schema, isNumber field]
  where
    go (x, y) [] = [NumPos x y y]
    go (x, y) nums@(NumPos xNum ylNum yrNum : ixsrem) 
        | x == xNum && y+1 == ylNum = NumPos xNum y yrNum : ixsrem
        | otherwise = NumPos x y y : nums 

isAdjacent :: Schematic -> NumPos -> Bool
isAdjacent schema (NumPos numX numYl numYr) = any check adjIdx
    where adjIdx = [(x, y) | x <- [numX-1..numX+1], y <- [numYl-1 .. numYr+1]]
          check = maybe False isSymbol . (schema !?)

toInt :: Schematic -> NumPos -> Int
toInt schema (NumPos x yl yr) = decimalToInt [ fromDigit $ schema ! (x, y) | y <- [yl .. yr] ]
    where fromDigit (Number n) = n
          fromDigit _ = error "Invalid Number Position!"

part1 :: Schematic -> Int
part1 schema = sum $ map (toInt schema) $ filter (isAdjacent schema) numbers
    where numbers = getNumbers schema

part2 :: Schematic -> Int
part2 _ = -1

