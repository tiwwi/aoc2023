{-# LANGUAGE OverloadedLists #-}
module Day12 (solveFrom) where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Attoparsec.Text
import Helpers (quickParseT)
import Control.Applicative ((<|>))

import Data.Array.IArray
import qualified Data.Vector as V

import Data.List (intercalate)
import Data.Bifunctor (first, bimap)

data Entry = Any | On | Off deriving (Show, Eq)
type SpringRecord = [Entry]
type SpringBackup = [Int]

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 values, show $ part2 values)
    where values = quickParseT (springP `sepBy1` endOfLine) txt

springP :: Parser (SpringRecord, SpringBackup)
springP = (,) <$> recordP <*> (space *> backupP)
    where recordP = many1 $ (On <$ char '#') <|> (Off <$ char '.') <|> (Any <$ char '?')
          backupP = decimal `sepBy1` char ','

matchS :: Entry -> Entry -> Bool
matchS On Off = False
matchS Off On = False
matchS _ _ = True

arrangementsDP :: SpringRecord -> SpringBackup -> Int
arrangementsDP record backup = resultsMap ! (0,0)
    where
        recordV = V.fromList record
        backupV = V.fromList backup
        recordLen = V.length recordV
        backupLen = V.length recordV

        resultsMap :: Array (Int,Int) Int
        resultsMap = genArray ((0,0),(recordLen, backupLen)) numArr
        numArr (i,j) 
            | V.null bupV = fromEnum $ V.all (/= On) recV
            | V.null recV = 0
            | otherwise = skipValue + takeValue
            where recV = V.drop i recordV
                  bupV = V.drop j backupV
                  r = V.head recV
                  b = V.head bupV
                  skipValue = if r /= On then resultsMap ! (i+1, j) else 0
                  canTake = V.and $ V.zipWith matchS recV (V.replicate b On V.++ [Off])
                  takeValue = if canTake then resultsMap ! (i+b+1,j+1) else 0

part1, part2 :: [(SpringRecord, SpringBackup)] -> Int
part1 = sum . map (uncurry arrangementsDP . first (++ [Off]))
part2 = part1 . map (bimap (intercalate [Any] . replicate 5) (concat . replicate 5))
