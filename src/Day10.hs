module Day10 (solveFrom) where

import Data.Array.IArray
import Linear.V2
import Data.List(find, sort, groupBy, foldl')
import Data.Maybe
import Data.Function (on)
import Control.Lens((^.))
import qualified Data.Set as S
import Helpers (replaceAll)
import Debug.Trace

type Pos = V2 Int
type Offset = V2 Int

type Maze = Array Pos Char

up, down, left, right :: Offset
up = V2 (-1) 0
down = V2 1 0
left = V2 0 (-1)
right = V2 0 1

directions :: [Offset]
directions = [up, down, left, right]

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . readFile

solve :: String -> (String, String)
solve txt = (show $ part1 cyc, show $ part2 maze cyc)
    where maze = parseMaze txt
          cyc = getCycle maze

parseMaze :: String -> Maze
parseMaze txt = listArray bds $ concat lns
  where
    lns = lines txt
    nCols = length $ head lns
    nRows = length lns
    bds = (V2 1 1, V2 nRows nCols)

pipeify :: Char -> Maybe [Offset]
pipeify '|' = Just [up, down]
pipeify '-' = Just [left, right]
pipeify 'L' = Just [up, right]
pipeify 'J' = Just [up, left]
pipeify '7' = Just [down, left]
pipeify 'F' = Just [down, right]
pipeify '.' = Nothing
pipeify 'S' = Nothing
pipeify _ = error "Unknown Symbol"

findUnsafe :: (a -> Bool) -> [a] -> a
findUnsafe = (fromJust .) . find

walk :: Maze -> (Pos, Pos) -> [Pos]
walk maze (lst, cur) 
    | here == 'S' = []
    |otherwise = cur:walk maze (cur, next)
        where here = maze ! cur
              next = fromJust $ reachable maze cur >>= (find (/= lst))

reachable :: Maze -> Pos -> Maybe [Pos]
reachable maze pos = map (pos+) <$> (pipeify $ maze ! pos)

getStartPositions :: Maze -> (Pos, [Pos])
getStartPositions maze = (start, pipes)
    where start = findUnsafe ((=='S') . (maze!)) $ indices maze
          adjacent = map (start+) directions
          pipes = filter (maybe False (start `elem`) . reachable maze) adjacent

getCycle :: Maze -> [Pos]
getCycle maze = start:walk maze (start, firstPipe)
    where (start, pipes) = getStartPositions maze
          firstPipe = head pipes


depipeify :: [Offset] -> Maybe Char
depipeify xs = lookup (sort xs) depipeTable
    where depipeTable = mapMaybe (\x -> (,x) . sort <$> pipeify x) "|-LJ7F"

part1 = (`div` 2) . length

part2 maze cyc = sum $ snd . rowValue <$> rowsTransformed
    where toReplace = [("FJ","|"), ("L7","|"), ("F7", ""), ("LJ", "")]
          rows = groupBy ((==) `on` (^. _x) . fst) (assocs maze)
          rowModify (pos, chr) = if (S.member pos cycleSet) then chr else '.'
          (start, pipes) = getStartPositions maze
          newS = pure $ fromJust $ depipeify $ subtract start <$> pipes

          rowsTransformed = replaceAll toReplace . filter (not . (`elem` "-")) . replaceAll [("S", newS)] . map rowModify <$> rows

          cycleSet = S.fromList cyc
          go (True, c) '.' = (True, c+1)
          go (False, c) '.' = (False, c)
          go (b, c) '|' = (not b, c)

          rowValue = foldl' go (False, 0)

