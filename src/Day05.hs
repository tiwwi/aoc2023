{-# LANGUAGE OverloadedStrings #-}
module Day05 (solveFrom) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M
import Data.Attoparsec.Text as A
import Data.Char (isSpace)
import Control.Monad (guard)
import Data.Maybe

data AgriBlock = AgriBlock {dest::Int, source::Int, range::Int} deriving(Show)
type AgriMap = M.Map Int AgriBlock
type Seeds = [Int]

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 agri seeds, show $ part2 agri seeds)
    where (seeds, agri) = case parseOnly agrP txt of
                    Right m -> m
                    Left err -> error err

agrP :: Parser (Seeds, [AgriMap])
agrP = (,) <$> seedP <*> (skipSpace *> mapP `sepBy1` (endOfLine *> endOfLine))

seedP :: Parser Seeds
seedP = string "seeds: " *> (decimal `sepBy1` space)

mapP :: Parser AgriMap
mapP = toMap <$> 
    (skipWhile (not . isSpace) *> string " map:" *> endOfLine *> (blockP `sepBy1` endOfLine))
    where toMap blocks = M.fromList $ zip (source <$> blocks) blocks

blockP :: Parser AgriBlock
blockP = do
    [a,b,c] <- decimal `sepBy` char ' '
    return $ AgriBlock a b c

throughMap :: Int -> AgriMap -> Int
throughMap n mp = fromMaybe n $ do
    AgriBlock dest source range <- snd <$> M.lookupLE n mp
    guard $ n < (source + range)
    return $ n - source + dest

throughMaps :: [AgriMap] -> Int -> Int
throughMaps maps n = foldl throughMap n maps

part1 :: [AgriMap] -> Seeds -> Int
part1 maps seeds = minimum $ map (throughMaps maps) seeds

part2 :: [AgriMap] -> Seeds -> Int
part2 maps seeds = part1 maps newSeeds
    where tupleChunks [] = []
          tupleChunks [_] = error "Seeds list has odd size!"
          tupleChunks (x:y:xs) = (x,y):tupleChunks xs
          toRange (x,y) = [x .. x+y-1]
          newSeeds = tupleChunks seeds >>= toRange

