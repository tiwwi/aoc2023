module Day21 (solveFrom) where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Array.Unboxed
import Data.Array.ST

import Linear.V2
import Helpers (readMatrix)

import qualified Data.Sequence as SQ
import Data.List (find)

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Maybe
import Control.Monad.ST

import Data.Maybe

type Pos = V2 Int
type Garden = UArray Pos Bool
type BFSTable s = STUArray s Pos Int
type BFSResults = UArray Pos Int
type BFSQueue = SQ.Seq Pos
type BFSMonad s = RWST (Garden, BFSTable s) () BFSQueue (ST s)

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 results 64, show $ part2 results 26501365)
    where garden = array (bounds tmpGarden) $ (fmap (/='#') <$> assocs tmpGarden)
          tmpGarden = readMatrix txt
          Just (start, _) = find ((=='S') . snd) $ assocs tmpGarden
          results = getResults garden start

up, down, left, right :: V2 Int
up = V2 (-1) 0
down = V2 1 0
left = V2 0 (-1)
right = V2 0 1

cardinals :: [V2 Int]
cardinals = [up, down, left, right]

dequeue :: MaybeT (BFSMonad s) Pos
dequeue = do
    (q SQ.:<| qs) <- lift $ get
    lift $ put qs
    return q

enqueue :: Pos -> BFSMonad s ()
enqueue pos = modify (SQ.|> pos)

neighbours :: Pos -> BFSMonad s [Pos]
neighbours pos = do
    (garden, table) <- ask
    let nbs = [nb | nb <- (pos+) <$> cardinals, fromMaybe False (garden !? nb)]
    filterM (fmap (== -1) . lift . readArray table) nbs

addNeighbours :: Pos -> BFSMonad s ()
addNeighbours pos = do
    toVisit <- neighbours pos
    table <- asks snd
    curDist <- lift $ readArray table pos
    forM_ toVisit $ \nb -> do
        lift $ writeArray table nb (curDist + 1)
        enqueue nb

bfsStep :: BFSMonad s Bool
bfsStep = do
    cur <- runMaybeT dequeue
    case cur of
        Just p -> do
            addNeighbours p
            return False
        Nothing -> return True

bfs :: BFSMonad s (BFSTable s)
bfs = (bfsStep >>= (flip unless (void bfs))) >> asks snd

getResults :: Garden -> Pos -> BFSResults
getResults garden start = results
    where bfsFun = runRWST bfs
          startQueue = SQ.singleton start
          results = runSTUArray $ do
              startArr <- newArray (bounds garden) (-1)
              writeArray startArr start 0
              (table, _, _)<- bfsFun (garden, startArr) startQueue
              return table

part1 :: BFSResults -> Int -> Int
part1 results nsteps = length $ filter (\n -> n >= 0 && n <= nsteps && n `mod` 2 == nsteps `mod` 2) $ elems results

part2 :: BFSResults -> Int -> Int
part2 results nsteps = n^2 * nAllEven - n*nOutsideEven + (n-1)^2 * nAllOdd + (n-1) * nOutsideOdd
    where n = (1 + ((nsteps - 65) `div` 131))
          allPoints = filter ((>= 0) . snd) $ assocs results
          allEvenPoints = filter ((== mod nsteps 2) . (`mod` 2) . snd) $ allPoints
          allOddPoints = filter ((/= mod nsteps 2) . (`mod` 2) . snd) $ allPoints
          outsideOddPoints =  [()| (V2 p1 p2, i) <- allOddPoints, abs (p1 - 65) + abs (p2 - 65) > 65]
          outsideEvenPoints = filter ((> 65) . snd) allEvenPoints
          nAllEven = length $ allEvenPoints
          nAllOdd = length $ allOddPoints 
          nOutsideEven = length $ outsideEvenPoints
          nOutsideOdd= length $ outsideOddPoints
