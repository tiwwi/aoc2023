{-# LANGUAGE OverloadedStrings #-}
module Day08 (solveFrom) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text as A
import qualified Data.Map.Strict as M
import Data.Char (isUpperCase)
import Data.List (findIndex, scanl')
import Control.Applicative((<|>))
import Control.Monad (mfilter)
import Helpers


type Place = T.Text
data Node = Node {name::Place, left::Place, right::Place}
data Dir = L | R deriving (Show, Read)

type Path = [Dir]
type Network = M.Map Place Node

-- Boilerplate
solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 path network, show $ part2 path network)
    where (path, network) = quickParseT networkP txt

networkP :: Parser (Path, Network)
networkP = (,) <$> (many1 dirP) <*> (skipSpace *> fmap toMap (nodeP `sepBy1` endOfLine))
    where toMap = M.fromList . map (\node -> (name node, node))

dirP :: Parser Dir
dirP = read . pure <$> (char 'L' <|> char 'R')

nodeP :: Parser Node
nodeP = Node <$> nameP 
             <*> (string " = (" *> nameP)
             <*> (string ", " *> nameP <* ")")

nameP :: Parser Place
nameP = mfilter (T.all isUpperCase) $ A.take 3

getNext :: Dir -> Node -> Place
getNext L = left
getNext R = right

pathStep :: Network -> Place -> Dir -> Place
pathStep net loc dir = getNext dir $ net M.! loc

findLen :: (Place -> Bool) -> Network -> Path -> Place -> Maybe Int
findLen f network path start = findIndex f $ scanl' (pathStep network) start (cycle path)

part1, part2 :: Path -> Network -> Maybe Int
part1 path network = findLen (== "ZZZ") network path "AAA"
part2 path network = foldr1 lcm <$> mapM (findLen ((== 'Z') . T.last) network path) starts
    where starts = filter ((== 'A') . T.last) $ M.keys network
