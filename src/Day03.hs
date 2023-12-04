module Day03 (solveFrom) where

import Data.Array.IArray
import Data.Char (digitToInt, isDigit)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Maybe (mapMaybe)
import Data.List (nub)
import Control.Monad (guard)
import Helpers (decimalToInt)
import qualified Data.Set as S

data Field = Symbol Char | Number Int | Empty deriving (Show, Eq)
data NumPos = NumPos Int Int Int deriving(Show)

instance Eq NumPos where
    NumPos x1 y1 _ == NumPos x2 y2 _ = x1 == x2 && y1 == y2
instance Ord NumPos where
    NumPos x1 y1 _ <= NumPos x2 y2 _ = x1 < x2 || x1 == x2 && y1 <= y2

type Pos = (Int, Int)
type Schematic = Array Pos Field

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 schema numbers, show $ part2 schema numbers)
    where schema = parseSchematic txt
          numbers = getNumbers schema

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

isContained :: Pos -> NumPos -> Bool
isContained (x,y) (NumPos xn yl yr) = x == xn && yl <= y && y <= yr

part1 :: Schematic -> [NumPos] -> Int
part1 schema numbers = sum $ map (toInt schema) $ filter (isAdjacent schema) numbers

part2 :: Schematic -> [NumPos] -> Int
part2 schema numbers = sum $ map starScore $ stars
    where isStar (Symbol '*') = True
          isStar _ = False
          numberSet = S.fromList numbers
          stars = [ idx | (idx, f) <- assocs schema, isStar f ]
          neighbours (x,y) = tail [(x', y') | x' <- [x, x-1, x+1], y' <- [y, y-1, y+1]]
          checkAndGet (x,y) = do
              num <- S.lookupLE (NumPos x y y) numberSet
              guard $ isContained (x,y) num
              return num
          starScore (x,y) = let candidates = nub $ mapMaybe checkAndGet $ neighbours (x,y)
                             in if length candidates == 2
                                then product $ toInt schema <$> candidates 
                                else 0

