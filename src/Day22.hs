{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Day22 (solveFrom) where

import Debug.Trace

import Linear.V2
import Linear.V3

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Attoparsec.Text as A
import Helpers

import Control.Lens
import qualified Data.Set as S

import Control.Monad (foldM)
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST

import Data.Bifunctor
import Data.List (sortOn, nub)

data Brick = Brick { _lo::V3 Int, _hi::V3 Int } deriving(Show, Eq, Ord)
type Tower s = STArray s (V2 Int) [(Int, Brick)]

makeLenses ''Brick

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 removeable, show $ part2 removeable dropped)
    where bricks = quickParseT (brickP `A.sepBy1` A.endOfLine) txt
          (removeable, dropped) = dropBricks bricks

v3P :: A.Parser (V3 Int)
v3P = do
    [x,y,z] <- A.decimal `A.sepBy1` (A.char ',')
    pure $ V3 x y z

brickP :: A.Parser Brick
brickP = Brick <$> v3P <* A.char '~' <*> v3P

expand2D :: Brick -> [V2 Int]
expand2D (Brick lo hi) = takeUntil (== hixy) $ iterate (+diff) loxy
    where loxy = lo ^. _xy
          hixy = hi ^. _xy
          diff = signum <$> hixy - loxy

brickHeight :: Brick -> Int
brickHeight (Brick (V3 _ _ loh) (V3 _ _ hih)) = 1 + hih - loh

stackBrick :: Tower s -> (S.Set Brick, S.Set Brick) -> Brick -> ST s (S.Set Brick, S.Set Brick)
stackBrick tower (disintegrate, dropped) brick = do
    let brickIndices = expand2D brick
    bricksBelow <- mapM (fmap head . readArray tower) $ brickIndices
    let maxHeight = maximum $ fst <$> bricksBelow
        brickH = brickHeight brick
        droppedHeight = maxHeight + (brickH)
        droppedBrick = lo._z .~ (maxHeight + 1) $ hi._z .~ (maxHeight + brickH) $ brick
        bricksLandedOn = nub $ [ brick' | (height', brick') <- bricksBelow, height' == maxHeight ]

    mapM_ (flip (modifyArray tower) ((droppedHeight, droppedBrick):)) brickIndices
    let updatedDisintegrate = case bricksLandedOn of
            [x] -> S.delete x disintegrate
            _   -> disintegrate
    return (S.insert droppedBrick updatedDisintegrate, S.insert droppedBrick dropped)

runBricks :: (V2 Int, V2 Int) -> [Brick] -> ST s (S.Set Brick, S.Set Brick)
runBricks bds@(V2 minx miny, V2 maxx maxy) bricks = do
    let defaultBrick = Brick (V3 minx miny 0) (V3 maxx maxy 0)
    let sortedBricks = sortOn (view $ lo._z) bricks
    tower <- newArray bds [(0, defaultBrick)]
    foldM (stackBrick tower) (S.empty, S.empty) sortedBricks

dropBricks :: [Brick] -> (S.Set Brick, S.Set Brick)
dropBricks bricks = runST (runBricks bds bricks)
    where minx = minimum $ (view $ lo._x) <$> bricks
          miny = minimum $ (view $ lo._y) <$> bricks
          maxx = maximum $ (view $ hi._x) <$> bricks
          maxy = maximum $ (view $ hi._y) <$> bricks
          bds = (V2 minx miny, V2 maxx maxy)

part1 :: S.Set Brick -> Int
part1 = S.size
part2 :: S.Set Brick -> S.Set Brick -> Int
part2 boring dropped = sum $ traceShowId $ dropAfterDelete <$> (S.toList toCheck)
    where toCheck = S.difference dropped boring
          droppedSort = sortOn (view $ lo._z) (S.toList dropped)
          sortOnZ = sortOn (view $ lo._z)
          dropAfterDelete :: Brick -> Int
          dropAfterDelete brick = S.size $ S.difference (snd $ dropBricks $ sortOnZ $ filter (/= brick) droppedSort) dropped
