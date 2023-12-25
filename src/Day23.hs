{-# LANGUAGE MonadComprehensions #-}
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

import qualified Data.Vector as V

import Data.Bifunctor
import Control.Monad.State
import Control.Monad (when)

data FakeGraph = FNode Pos [(Int, FakeGraph)] | Goal Pos deriving(Show)
data Field = Empty | Wall | Slope (V2 Int) deriving(Show, Eq)
type Vertex = Int
type Weight = Int
type Graph = V.Vector (V.Vector (Weight, Vertex))
type Pos = V2 Int
type Laby = Array Pos Field


solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 ezGraph, show $ part2 graph goal start)
    where laby = charToField <$> readMatrix txt
          (ezGraph, startPos, goalPos) = buildEzGraph laby
          (graph, start, goal) = mapGraphToVecGraph startPos goalPos $ collectGraph M.empty ezGraph


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

buildEzGraph :: Laby -> (FakeGraph, Pos, Pos)
buildEzGraph laby = ((execState (graphViaDfs laby goal start) M.empty) M.! start, start, goal)
    where (_, V2 xhi yhi) = bounds laby
          start = fst $ fromJust $ find (\((V2 x y), field) -> field == Empty && x == 1) $ assocs laby
          goal = fst $ fromJust $ find (\((V2 x y), field) -> field == Empty && x == xhi) $ assocs laby

longestFakePath :: FakeGraph -> Int
longestFakePath (Goal _ ) = 0
longestFakePath (FNode _ below) = maximum $ [ len + longestFakePath node | (len, node) <- below ]

collectGraph :: M.Map Pos [(Int, Pos)] -> FakeGraph -> M.Map Pos [(Int, Pos)]
collectGraph mp (Goal pos) = M.insertWith (++) pos [] mp
collectGraph mp (FNode pos below) = if M.notMember pos mp
                                        then M.insertWith (++) pos [(i, getPos n) | (i,n) <- below] updatedNbs
                                        else mp
    where updatedNbs = foldl (\mp' (len, fake) -> M.insertWith (++) (getPos fake) [(len, pos)] (collectGraph mp' fake)) mp below
          getPos (Goal p) = p
          getPos (FNode p _) = p

mapGraphToVecGraph :: Pos -> Pos -> M.Map Pos [(Int, Pos)] -> (Graph, Int, Int)
mapGraphToVecGraph start goal mp = (V.fromList [ V.fromList [(len, asIndices M.! nb) | (len, nb) <- nbs] | (_, nbs) <- mpList],
                                    asIndices M.! start,
                                    asIndices M.! goal)
    where mpList = M.toList mp
          (_, asIndices) = M.mapAccum (\i _pos -> (i+1, i)) 0 mp

longestPath :: Graph -> Int -> S.Set Vertex -> Vertex -> Int -> State Weight ()
longestPath graph goal visited cur pathWeight
    | cur == goal = modify (max pathWeight)
    | not $ canReachGoal withCur cur = return ()
    | otherwise = do
        let potentialWeight = pathWeight + V.sum [ V.maximum $ V.cons 0 [ w | (w,j) <- edges, S.notMember j visited ] | (i,edges) <- V.indexed graph, S.notMember i withCur ]
        currentMax <- get
        when (potentialWeight > currentMax) $ 
            V.sequence_ [ longestPath graph goal withCur next (pathWeight + weight) | (weight, next) <- (graph V.! cur), S.notMember next visited ]
    where withCur = S.insert cur visited
          neighbours v = graph V.! v

          canReachGoal :: S.Set Vertex -> Vertex -> Bool
          canReachGoal visited' vertex 
              | vertex == goal = True
              | otherwise = V.any (canReachGoal withVertex) $ [ next | (_, next) <- neighbours vertex, S.notMember next withVertex ]
              where withVertex = S.insert vertex visited'
          
part1 = longestFakePath
part2 graph goal start = execState (longestPath graph goal S.empty start 0) (-1)

