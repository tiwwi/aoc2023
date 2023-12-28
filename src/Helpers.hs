module Helpers (
    readT,
    readMaybeT,
    decimalToInt,
    quickParseT,
    replaceAll,
    count,
    readMatrix,
    arrayRows,
    arrayColumns,
    takeUntil,
    pairs,
) where

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as T
import Data.List (isPrefixOf, find)
import Text.Read (readMaybe)
import Linear.V2 (V2(V2))
import Data.Array.IArray
import Data.List (tails)

readT :: Read a => T.Text -> a
readT = read . T.unpack

readMaybeT :: Read a => T.Text -> Maybe a
readMaybeT = readMaybe . T.unpack

decimalToInt :: [Int] -> Int
decimalToInt = foldl go 0
    where go acc x = 10*acc + x

quickParseT :: T.Parser a -> T.Text -> a
quickParseT p txt = case T.parseOnly p txt of
                        Right a -> a
                        Left err -> error err

replaceAll :: (Eq a, Foldable t, Show a) => t ([a], [a]) -> [a] -> [a]
replaceAll _ [] = []
replaceAll patterns xs@(x:re) = maybe (x:replaceAll patterns re) replaceStart replacer
    where replacer = find ((`isPrefixOf` xs) . fst) patterns
          replaceStart (from,to) = to ++ replaceAll patterns (drop (length from) xs)

readMatrix :: T.Text -> Array (V2 Int) Char
readMatrix txt = listArray (V2 1 1, V2 nRows nCols) $ lns >>= T.unpack
    where lns = T.lines txt
          nRows = length lns
          nCols = T.length $ head lns

arrayColumns :: Array (V2 Int) a -> [[a]]
arrayColumns arr = [ [ arr ! V2 i j | i <- [lox .. hix] ] | j <- [loy..hiy] ]
    where (V2 lox loy, V2 hix hiy) = bounds arr

arrayRows :: Array (V2 Int) a -> [[a]]
arrayRows arr = [ [ arr ! V2 i j | j <- [loy .. hiy] ] | i <- [lox..hix] ]
    where (V2 lox loy, V2 hix hiy) = bounds arr

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x:xs)
    | f x = [x]
    | otherwise = x:takeUntil f xs

pairs :: [a] -> [(a, a)]
pairs xs = [ (x,y) | (x:ys) <- tails xs, y <- ys ]

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f
