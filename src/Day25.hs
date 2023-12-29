{-# LANGUAGE OverloadedStrings #-}
module Day25 (solveFrom) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text as A
import Helpers

import Data.Char (isLower)
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.Trans.Maybe

import System.Random

import Data.Foldable(toList)
import Data.Bits (xor)
import Data.Tuple (swap)
import Data.List (unfoldr, find)
import Data.Graph
import Data.Array.Unboxed
import Data.Array.ST

type Name = T.Text
type ResidualCapas s = STUArray s (Vertex, Vertex) Float

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 graph, "Christmas is saved! ... unless?")
    where values = quickParseT (edgeP `sepBy1` endOfLine) txt
          graph = toGraph values

nameP :: Parser Name
nameP = mfilter ((==3) . T.length) (A.takeWhile isLower)
edgeP :: Parser (T.Text, [T.Text])
edgeP = (,) <$> nameP <*> (": " *> sepBy1 nameP " ")

toGraph :: [(T.Text, [T.Text])] -> Graph
toGraph edges = buildG (1, nVerts) $ do
          (l, r) <- pairs
          let intEdge = (getInt l, getInt r)
          [intEdge, swap intEdge]
    where pairs = edges >>= uncurry (fmap . (,))
          vertexSet = foldr (\(l, r) s -> S.insert r $ S.insert l s) S.empty pairs
          nVerts = S.size vertexSet
          asInt = M.fromList $ zip (S.toList vertexSet) [1..] :: M.Map T.Text Int
          getInt = (asInt M.!)

findM :: MonadPlus m => (a -> m Bool) -> [m a] -> m a
findM f [] = mzero
findM f (x:xs) = mzero

fulkersonDFS :: Vertex -> Vertex ->  MaybeT (StateT (S.Set Vertex) (ReaderT (Graph, ResidualCapas s) (ST s))) [Edge]
fulkersonDFS goal start = if start == goal then pure [] else do
    notVisited <- gets $ S.notMember start
    guard notVisited
    modify $ S.insert start
    (graph, weights) <- ask
    let neighbours = graph ! start
    viableNbs <- filterM (lift . lift . lift . fmap (> 0) . readArray weights . (start,) ) neighbours
    (nb, l) <- msum [ (nb,) <$> fulkersonDFS goal nb | nb <- viableNbs ]
    return $ (start,nb):l

fordFulkersonM :: Vertex -> Vertex -> MaybeT (ReaderT (Graph, ResidualCapas s) (ST s)) ()
fordFulkersonM start goal = do
    dfsPath <- MaybeT $ evalStateT (runMaybeT $ fulkersonDFS goal start) S.empty
    capas <- asks snd
    minCap <- maximum <$> mapM (lift . lift . readArray capas) dfsPath
    forM_ dfsPath $ \(u,v) -> do
        lift $ lift $ modifyArray capas (u,v) (subtract minCap)
        lift $ lift $ modifyArray capas (v,u) (+ minCap)
    fordFulkersonM start goal

-- Returns the capacities in the residual graph
runFordFulkerson :: Graph -> Vertex -> Vertex -> UArray (Vertex, Vertex) Float
runFordFulkerson graph start goal = runST runFord
    where (_, nVerts) = bounds graph
          initialArray :: UArray Edge Float
          initialArray = array ((1,1), (nVerts, nVerts)) ([((ix, iy), 0) | ix <- indices graph, iy <- indices graph] ++ [(e, 1) | e <- edges graph])
          runFord = do
              capas <- thaw initialArray
              void $ runReaderT (runMaybeT $ fordFulkersonM start goal) (graph, capas)
              freeze capas

getSeparator :: Graph -> Vertex -> Vertex -> [Edge]
getSeparator graph start goal = filter (\(x,y) -> S.member x c1 `xor` S.member y c1) $ edges graph
    where capas = runFordFulkerson graph start goal
          (_, nVerts) = bounds graph
          newGraph = buildG (1, nVerts) $ [ (x,y) | x <- vertices graph, y <- vertices graph, let cap = capas ! (x,y), cap > 0 ]
          (c1:_) = S.fromList . toList <$> scc newGraph

part1 graph = do
    sep <- separator
    let nGraph = buildG (1, nVerts) $ filter (not . (`elem` sep)) $ edges graph
    pure $ product $ length <$> scc nGraph
    where (_, nVerts) = bounds graph
          rolls seed = unfoldr (Just . uniformR (1, nVerts)) $ mkStdGen seed
          rollList = zip (rolls 12345) (rolls 87103)
          separator = find ((==6) . length) $ uncurry (getSeparator graph) <$> rollList
