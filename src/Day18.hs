{-# LANGUAGE OverloadedStrings #-}
module Day18 (solveFrom) where

import Helpers
import Data.Attoparsec.Text qualified as A
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Linear.V2
import Linear.Vector

data Dir = U | R | D | L deriving(Show, Eq, Ord)
data Dig = Dig {dir::Dir, len::Int} deriving (Show)
type Pos = V2 Int

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part dig1, show $ part dig2)
    where (dig1, dig2) = unzip $ quickParseT (digP `A.sepBy1` A.endOfLine) txt

up, down, left, right :: V2 Int
up = V2 (-1) 0
down = V2 1 0
left = V2 0 (-1)
right = V2 0 1

toVec :: Dir -> Pos
toVec U = up
toVec D = down
toVec L = left
toVec R = right

digP :: A.Parser (Dig, Dig)
digP = (,)
            <$> digP1
            <*> (A.space *> digP2)

digP1, digP2 :: A.Parser Dig
digP1 = Dig 
            <$> A.choice (zipWith (<$) [U, R, D, L] (A.char <$> "URDL")) 
            <*> (A.space *> A.decimal) 
digP2 = flip Dig
            <$> ("(#" *> (quickParseT A.hexadecimal <$> A.take 5))
            <*> A.choice (zipWith (<$) [R, D, L, U] (A.char <$> "0123")) <* ")"

getPoints :: Pos -> [Dig] -> [Pos]
getPoints start = scanl (+) start . map (liftA2 (*^) len (toVec . dir))

tieLace :: [Pos] -> Int
tieLace xs = (`div` 2) $ abs $ sum $ zipWith go xs (tail xs ++ take 1 xs)
    where go (V2 x1 y1) (V2 x2 y2) = x1*y2 - y1*x2


part :: [Dig] -> Int
part xs = 1 + ((`div` 2) $ sum $ len <$> xs) + (tieLace $ getPoints (V2 0 0) xs)
