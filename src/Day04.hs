{-# LANGUAGE OverloadedStrings #-}

module Day04 (solveFrom) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as S
import Data.Attoparsec.Text as A
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

data Card = Card Int [Int] [Int] deriving (Show)
data TinyCard = TinyCard {idx::Int, wins::Int} deriving (Show)

type Bingo = [TinyCard]

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 bingo, show $ part2 bingo)
    where bingo = case parseOnly boardP txt of
                    Right b -> b
                    Left err -> error err

boardP :: Parser Bingo
boardP = (shrinkCard <$> cardP) `sepBy1` endOfLine

cardP :: Parser Card
cardP = Card
    <$> (string "Card" *> skipSpace *> decimal)
    <*> (char ':' *> skipSpace *> (decimal `sepBy1` skipSpace))
    <*> (string " |" *> skipSpace *> (decimal `sepBy1` skipSpace))

shrinkCard :: Card -> TinyCard
shrinkCard (Card idx win mine) = TinyCard idx nWins
    where nWins = S.size $ S.fromList win `S.intersection` S.fromList mine

part1 :: Bingo -> Int
part1 = sum . map (toPoints . wins) 
    where toPoints n = 2^n `div` 2

part2 :: Bingo -> Int
part2 bingo = V.sum $ foldl go cardTracker bingo
    where cardTracker = V.replicate (length bingo) 1
          go track card = V.modify (addCards card) track
          addCards (TinyCard idx wins) v = do
              n <- MV.read v (idx - 1)
              let lastIdx = min wins (MV.length v - idx) - 1
              mapM_ (MV.modify v (+n)) [idx .. idx + lastIdx]

