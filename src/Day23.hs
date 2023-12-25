{-# LANGUAGE PartialTypeSignatures #-}
module Day23 (solveFrom) where

import Helpers
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Linear.V2
import Data.Array.IArray
import Data.Maybe
import Data.List (find)

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Graph
import Data.Bifunctor
import Control.Monad.State
import Debug.Trace

data FakeGraph = FNode Pos [(Int, FakeGraph)] | Goal Pos deriving(Show)
data Field = Empty | Wall | Slope (V2 Int) deriving(Show, Eq)
type Pos = V2 Int
type Laby = Array Pos Field


solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 ezGraph, show $ part2 ezGraph)
    where laby = charToField <$> readMatrix txt
          ezGraph = buildEzGraph laby


up, down, left, right :: V2 Int
up = V2 (-1) 0
down = V2 1 0
left = V2 0 (-1)
right = V2 0 1

directions :: [V2 Int]
directions = [up, down, left, right]

charToField :: Char -> Field
charToField '#' = Wall
charToField '.' = Empty
charToField '>' = Slope right
charToField 'v' = Slope down
charToField '<' = Slope left
charToField '^' = Slope up
charToField _ = error "Unknown symbol!"

incFst :: (Int, a) -> (Int, a)
incFst = first (+1)

walkForwards :: Laby -> Pos -> (Pos, Pos) -> (Int, Pos)
walkForwards laby goal (last, cur) = case here of
        Slope dir -> incFst $ walkForwards laby goal (cur, cur + dir)
        Empty -> case viableNbs of
                    [next] -> if next == goal then (1, next) else incFst $ walkForwards laby goal (cur, next)
                    [] -> error "Ope"
                    _ -> (0, cur)
    where here = laby ! cur
          nbs = (cur+) <$> directions
          viableNbs = filter (\nb -> (maybe False (not . isWall) . (laby !? ) $ nb) && nb /= last) nbs

isWall :: Field -> Bool
isWall Wall = True
isWall _ = False

graphViaDfs :: Laby -> Pos -> Pos -> State (M.Map Pos FakeGraph) FakeGraph
graphViaDfs laby goal cur
    | cur == goal = modify (M.insert goal (Goal cur)) >> return (Goal cur)
    | otherwise = do
        foundGraph <- gets (M.!? cur)
        case foundGraph of
            Just fake -> return fake
            Nothing -> do
                let thisGraph = mapM (uncurry fmap . bimap (,) (graphViaDfs laby goal)) walked
                finishedGraph <- FNode cur <$> thisGraph
                modify (M.insert cur finishedGraph)
                return finishedGraph
            
    where (_, V2 hix hiy) = bounds laby
          viableNbs = filter isViable $ (cur+) <$> directions
          walked = incFst . walkForwards laby goal <$> (cur,) <$> viableNbs
          isViable nb = case laby !? nb of
                            Just (Slope dir) -> nb + dir /= cur
                            Just Empty -> True
                            _ -> False

buildEzGraph :: Laby -> FakeGraph
buildEzGraph laby = (execState (graphViaDfs laby goal start) M.empty) M.! start
    where (_, V2 xhi yhi) = bounds laby
          start = fst $ fromJust $ find (\((V2 x y), field) -> field == Empty && x == 1) $ assocs laby
          goal = fst $ fromJust $ find (\((V2 x y), field) -> field == Empty && x == xhi) $ assocs laby

longestFakePath :: FakeGraph -> Int
longestFakePath (Goal _ ) = 0
longestFakePath (FNode _ below) = maximum $ [ len + longestFakePath node | (len, node) <- below ]

part1 = longestFakePath
part2 = const 0

