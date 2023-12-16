module Day16 (solveFrom) where

import Helpers (readMatrix)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Array.IArray
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad (guard, msum)
import Linear.V2
import Linear.Metric
import Data.Set qualified as S

type Laby = Array (V2 Int) Tile
data Beam = Beam {pos::V2 Int, dir::V2 Int} deriving (Show, Eq, Ord)
data Tile = Empty | Asc | Des | Hor | Ver deriving (Show)

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 mirrors (Beam (V2 1 1) right), show $ part2 mirrors)
    where mirrors = charToTile <$> readMatrix txt :: Laby

charToTile :: Char -> Tile
charToTile '.' = Empty
charToTile '/' = Asc
charToTile '\\'= Des
charToTile '-' = Hor
charToTile '|' = Ver
charToTile _   = error "Unknown Tile!"

up, down, left, right :: V2 Int
up = V2 (-1) 0
down = V2 1 0
left = V2 0 (-1)
right = V2 0 1

stepBeam :: Beam -> Beam
stepBeam beam@(Beam pos dir) = beam { pos = pos+dir }
redirectBeam :: Beam -> V2 Int -> Beam
redirectBeam (Beam pos _) ndir = Beam (pos+ndir) ndir
                    
moveBeam :: Beam -> Tile -> [Beam]
moveBeam beam Empty = pure $ stepBeam beam
moveBeam beam@(Beam _ (V2 dirx diry)) Asc = let ndir = V2 (-diry) (-dirx) in pure $ redirectBeam beam ndir
moveBeam beam@(Beam _ (V2 dirx diry)) Des = let ndir = V2 diry dirx in pure $ redirectBeam beam ndir
moveBeam beam@(Beam _ dir) Hor 
    | dir `dot` right == 0 = redirectBeam beam <$> [left, right]
    | otherwise = pure $ stepBeam beam
moveBeam beam@(Beam _ dir) Ver
    | dir `dot` up == 0 = redirectBeam beam <$> [up, down]
    | otherwise = pure $ stepBeam beam

moveBeamSafe :: Beam -> Laby -> [Beam]
moveBeamSafe beam laby = filter (inRange (bounds laby) . pos) nextBeams
    where nextBeams = moveBeam beam (laby ! pos beam)

lift2 = lift . lift
calcBeam :: Beam -> MaybeT (ReaderT Laby (State (S.Set Beam))) ()
calcBeam beam = do
    isVisited <- lift2 $ gets $ S.member beam
    guard (not isVisited)
    lift2 $ modify $ S.insert beam
    nextBeams <- lift $ asks $ moveBeamSafe beam
    msum (calcBeam <$> nextBeams)

part1 :: Laby -> Beam -> Int
part1 laby start = S.size $ S.map pos results
    where results = snd $ runState (runReaderT (runMaybeT (calcBeam start)) laby) S.empty
part2 :: Laby -> Int
part2 laby = maximum $ part1 laby <$> startBeams
      where (_, V2 nRows nCols) = bounds laby
            startBeams = [Beam (V2 1 j) down | j <- [1..nCols]]
                         ++ [Beam (V2 nRows j) up | j <- [1..nCols]]
                         ++ [Beam (V2 i 1) right | i <- [1..nRows]]
                         ++ [Beam (V2 i nCols) left | i <- [1..nRows]]

