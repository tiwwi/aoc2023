module Day17 (solveFrom) where

import Helpers
import Linear.V2
import Data.Char (digitToInt)

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as S
import qualified Data.PQueue.Min as PQ
import Data.Array.IArray
import Data.List.NonEmpty qualified as N
import Data.List.NonEmpty (NonEmpty((:|)), (<|))


data Node = Node {pos::V2 Int, dir::V2 Int, remain::Int} deriving (Show, Eq, Ord)
data QNode = QNode(N.NonEmpty Node) Int deriving (Show, Eq)
data DijksQueue = DijksQueue {qu::(PQ.MinQueue QNode), reached::Maybe QNode, visited::S.Set Node, _nbfun::Nbfun}

type Nbfun = Node -> [Node]

type City = Array (V2 Int) Int

instance Ord QNode where
    compare (QNode _ x1) (QNode _ x2) = compare x1 x2

up, down, left, right :: V2 Int
up = V2 (-1) 0
down = V2 1 0
left = V2 0 (-1)
right = V2 0 1

directions :: [V2 Int]
directions = [up, down, left, right]

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 city, show $ part2 city)
    where city = digitToInt <$> readMatrix txt

neighbours :: Nbfun
neighbours (Node pos' dir' remain') = filter ((>= 0) . remain) $ (\d -> Node (pos'+d) d (if d == dir' then remain'-1 else 2)) <$> (filter (/= -dir') directions)

neighbours2 :: Nbfun
neighbours2 (Node pos' dir' remain')
    | 7 <= remain' && remain' <= 9 = [Node (pos' + dir') dir' (remain' - 1)]
    | otherwise = filter ( (>= 0) . remain) $ (\d -> Node (pos'+d) d (if d == dir' then remain'-1 else 9)) <$> (filter (/= -dir') directions) 

qnodeSucc :: Nbfun -> City -> QNode -> [QNode]
qnodeSucc f city (QNode path@(lst:|_) weight) = newQNode <$> nbs
    where nbs = filter (inRange (bounds city) . pos) $ f lst
          newQNode nb = QNode (nb <| path) (weight + city ! (pos nb))

du :: City -> V2 Int -> DijksQueue -> DijksQueue
du _ _ d@(DijksQueue _ (Just _) _ _) = d
du city goal d@(DijksQueue pqueue Nothing visited f) 
    | pos here == goal = d { reached = Just newMin }
    | S.member here visited = d { qu = newQueue }
    | otherwise = d { qu = nextQueue, visited = newVisited }
    where (newMin@(QNode (here :|_) _), newQueue) = PQ.deleteFindMin pqueue
          toAdd = qnodeSucc f city newMin
          nextQueue = foldr PQ.insert newQueue toAdd
          newVisited = S.insert here visited

runDijk :: Nbfun -> City -> Int
runDijk f city = weight
    where (_, goal) = bounds city
          start = DijksQueue (PQ.singleton $ QNode (N.singleton $ Node (V2 1 1) (V2 0 0) 3) 0) Nothing S.empty f
          Just (QNode _ weight) = reached $ until (isJust . reached) (du city goal) start

part1, part2 :: City -> Int
part1 = runDijk neighbours
part2 = runDijk neighbours2
