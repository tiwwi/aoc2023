module Day11 (solveFrom) where

import Data.Array.IArray
import qualified Data.Set as S
import Data.List (tails)
import Linear.V2
import Control.Lens

type Pos = V2 Int
type Universe = Array Pos Bool
data SparseU = SparseU {nRows :: Int, nCols:: Int, values::S.Set Pos}

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . readFile

solve :: String -> (String, String)
solve txt = (show $ part 2 universe, show $ part 1000000 universe)
    where universe = sparseify $ parseUniverse txt

parseUniverse :: String -> Universe
parseUniverse txt = listArray bds $ (=='#') <$> concat lns
  where
    lns = lines txt
    nCols = length $ head lns
    nRows = length lns
    bds = (V2 1 1, V2 nRows nCols)

sparseify :: Universe -> SparseU
sparseify uni = SparseU nRows nCols (S.fromList . map fst . filter snd $ assocs uni)
    where (V2 _ _, V2 nRows nCols) = bounds uni

setBetween :: Ord a => S.Set a -> (a, a) -> S.Set a
setBetween s (lo, hi) = fst $ S.split hi above
    where (_, above) = S.split lo s

part :: Int -> SparseU -> Int
part n (SparseU nRows nCols values) = sum $ [expandDist a b | (a:bs) <- tails valueList, b <- bs]
    where valueList = S.toList values
          emptyRows = S.fromAscList [1..nRows] `S.difference` S.map (^. _x) values
          emptyCols = S.fromAscList [1..nCols] `S.difference` S.map (^. _y) values
          expandDist (V2 x1 y1) (V2 x2 y2) = bigX - smX + (n - 1)*neRows + bigY - smY + (n - 1)*neCols
              where (smX, bigX, smY, bigY) = (min x1 x2, max x1 x2, min y1 y2, max y1 y2)
                    neRows = S.size $ setBetween emptyRows (smX, bigX)
                    neCols = S.size $ setBetween emptyCols (smY, bigY)

