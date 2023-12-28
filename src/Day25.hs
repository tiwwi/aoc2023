{-# LANGUAGE OverloadedStrings #-}
module Day25 (solveFrom) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text as A
import Helpers

import Data.Char (isLower)
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.Trans.Maybe

import Data.Tuple (swap)
import Data.Graph
import Data.Array.Unboxed
import Data.Array.IArray
import Data.Array.ST

type Name = T.Text
type AdjList = [Int]
type ResidualCapas s = STUArray s (Vertex, Vertex) Float

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap (solve) . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 graph, "Christmas is saved! ... unless?")
    where values = quickParseT (edgeP `sepBy1` endOfLine) txt
          graph = toGraph values

nameP :: Parser Name
nameP = (mfilter ((==3) . T.length) $ A.takeWhile isLower)
edgeP :: Parser (T.Text, [T.Text])
edgeP = (,) <$> (nameP) <*> (": " *> sepBy1 nameP " ")

toGraph :: [(T.Text, [T.Text])] -> Graph
toGraph edges = buildG (1, nVerts) $ do
          (l, r) <- pairs
          let intEdge = (getInt l, getInt r)
          [intEdge, swap intEdge]
    where pairs = edges >>= (uncurry $ fmap . (,))
          vertexSet = foldr (\(l, r) s -> S.insert r $ S.insert l s) S.empty pairs
          nVerts = S.size vertexSet
          asInt = M.fromList $ zip (S.toList vertexSet) [1..] :: M.Map T.Text Int
          getInt = (asInt M.!)

findM :: MonadPlus m => (a -> m Bool) -> [m a] -> m a
findM f [] = mzero
findM f (x:xs) = mzero

fulkersonDFS :: Vertex -> Vertex -> StateT (S.Set Vertex) (MaybeT (ReaderT (Graph, ResidualCapas s) (ST s))) [Edge]
fulkersonDFS goal start = if (traceShowId start) == goal then pure [] else do
    notVisited <- gets $ S.notMember start . (traceWith (show . S.size ))
    guard notVisited
    modify $ S.insert start
    (graph, weights) <- ask
    let neighbours = graph ! start
    viableNbs <- filterM (lift . lift . lift . fmap (> 0) . readArray weights . (start,) ) neighbours
    (nb, l) <- msum [ (nb,) <$> fulkersonDFS goal nb | nb <- viableNbs ]
    return $ (start,nb):l

fordFulkersonM :: Vertex -> Vertex -> MaybeT (ReaderT (Graph, ResidualCapas s) (ST s)) ()
fordFulkersonM start goal = do
    dfsPath <- evalStateT (fulkersonDFS goal (traceShow "STARTING" start)) S.empty
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
              capas <- thaw (initialArray)
              void $ runReaderT (runMaybeT $ fordFulkersonM start goal) (graph, capas)
              freeze capas

part1 graph = scc newGraph
    where capas = traceWith (show . bounds) $ runFordFulkerson graph 1 5
          (_, nVerts) = bounds graph
          newGraph = buildG (1, nVerts) $ [ (x,y) | x <- vertices graph, y <- vertices graph, let cap = capas ! (x,y), cap > 0 ]
          
