{-# LANGUAGE OverloadedStrings #-}
module Day20 (solveFrom) where

import Data.Attoparsec.Text as A
import Data.Text qualified as T
import Data.Text.IO qualified as T

import qualified Data.Sequence as SQ
import qualified Data.Map as M

import Data.Bifunctor

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.Strict
import Control.Monad.Trans.Maybe

import Data.Monoid
import Data.Maybe
import Data.Char

import Helpers
import Control.Applicative

type Name = T.Text

type Signal = Bool
data Module = Flip Signal | Conj (M.Map T.Text Signal) | Other deriving (Show)
data Node = Node {name::T.Text, modu::Module, wires::[Name]} deriving (Show)
data Pulse = Pulse {source::T.Text, target::T.Text, signal::Signal} deriving(Show)

type Graph = M.Map T.Text Node
type Queue = SQ.Seq Pulse

type GraphComputation = RWS () (Sum Int, Sum Int) (Graph, Queue)

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 graph, show $ part2)
    where graph = makeGraph $ quickParseT inputP txt

makeGraph :: [Node] -> Graph
makeGraph nodes = foldr go preGraph nodes
    where preGraph = M.fromList ( ("rx", Node "rx" Other []):[(name node, node) | node <- nodes])
          go node mp = foldr (uncurry M.insert) mp neighbours
              where neighbours = [ (oname, modifyNb (mp M.! oname))| oname <- wires node ]
                    modifyNb n@(Node _ (Conj mem) _) = n { modu = Conj $ M.insert (name node) False mem }
                    modifyNb n = n

inputP :: Parser [Node]
inputP = lineP `sepBy` endOfLine

nameP :: Parser Name
nameP = takeWhile1 isLower

lineP :: Parser Node
lineP = (\a b c -> Node b a c)
            <$> (fromMaybe Other <$> (optional (Flip False <$ char '%' <|> Conj M.empty <$ char '&')))
            <*> (nameP)
            <*> (" -> " *> (nameP `sepBy1` ", "))

runUntilComplete :: GraphComputation ()
runUntilComplete = runPulse >>= (flip unless runUntilComplete)

runPulse :: GraphComputation Bool
runPulse = do
    pulse <- runMaybeT dequeuePulse
    case pulse of
        Just (Pulse source target signal) -> do
            handleSignal signal source target
            return False
        Nothing -> return True

dequeuePulse :: MaybeT GraphComputation Pulse
dequeuePulse = do
    (q SQ.:<| qs) <- lift $ gets snd
    lift $ modify (second $ const qs)
    case signal q of
        True -> lift $ tell (1,0)
        False -> lift $ tell (0,1)
    return q

handleSignal :: Signal -> Name -> Name -> GraphComputation ()
handleSignal signal source target  = do
    node <- gets ((M.! target) . fst)
    let newModule = handleModuleSignal (modu node) signal source
    case newModule of
        Nothing -> return ()
        Just (newModule, newSignal) -> do
            let newNode@(Node _ _ newTargets) = node { modu = newModule }
            modify (first (M.insert target newNode))

            queue <- gets snd
            let newQueue = foldl (SQ.|>) queue pulses
                pulses = [Pulse target newTarget newSignal | newTarget <- newTargets]
            modify (second $ const newQueue)

handleModuleSignal :: Module -> Signal -> Name -> Maybe (Module, Signal)
handleModuleSignal (Other) _ _ = Nothing
handleModuleSignal (Flip _) True _ = Nothing
handleModuleSignal (Flip cur) False _ = Just (Flip $ newState, newState)
    where newState = not cur
handleModuleSignal (Conj incSignals) s source = Just $ (Conj updatedMap, not $ and updatedMap)
    where updatedMap = M.insert source s incSignals

part1 :: Graph -> Int
part1 graph = uncurry (*) $ bimap getSum getSum $ snd $ iterate graphStep (graph, (0, 0)) !! 1000
    where startQueue = SQ.fromList $ [ Pulse "broadcaster" target False | target <- wires $ graph M.! "broadcaster"]
          graphStep (graph', osteps) = let ((), ngraph, nsteps) = (runRWS runUntilComplete () (graph', startQueue)) in (fst ngraph, osteps <> nsteps <> (0,1))

-- Brain power
part2 :: Int
part2 = 262775362119547
