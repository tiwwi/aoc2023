{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Day19 (solveFrom) where

import Data.Attoparsec.Text as A
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Data.Map.Strict qualified as M

import Control.Lens

import Control.Applicative
import Control.Monad
import Data.Char (isLower)

import Data.Maybe
import Helpers

type Range = (Int, Int)

data Cmp = Less | Greater deriving (Show)
data Field = Xtreme | Musical | Aero | Shiny deriving (Show)
data Val = Reject | Accept | Other T.Text deriving (Show, Eq)
data Classifier = Class Field Cmp Int Val | Def Val deriving (Show)
data Work = Work {name::T.Text, checks::[Classifier]} deriving (Show)
data Part = Part {_x::Int, _m::Int, _a::Int, _s::Int} deriving (Show)
data PartRange = PartRange {_xrange::Range, _mrange::Range, _arange::Range, _srange:: Range} deriving(Show)


type Process = M.Map T.Text Work

makeLenses ''Part
makeLenses ''PartRange

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 work parts, show $ part2 work)
    where (workflows, parts) = quickParseT inputP txt
          work = toMap workflows

toMap :: [Work] -> Process
toMap works = M.fromList [(name w, w) | w <- works]

-- Parsing

word :: Parser T.Text
word = A.takeWhile isLower

cmpP :: Parser Cmp
cmpP = (Less <$ "<") <|> (Greater <$ ">")

valP :: Parser Val
valP = (Reject <$ "R") <|> (Accept <$ "A") <|> (Other <$> word)

fieldP :: Parser Field
fieldP = choice $ zipWith (<$) [Xtreme, Musical, Aero, Shiny] (char <$> "xmas")

classP :: Parser Classifier
classP =  (Class <$> fieldP <*> cmpP <*> decimal <*> (":" *> valP)) <|> (Def <$> valP)

workFlowP :: Parser Work
workFlowP = liftA2 Work word $ "{" *> (classP `sepBy1` ",") <* "}"

partP :: Parser Part
partP = "{" *> (Part 
            <$> ("x=" *> decimal) 
            <*> (",m=" *> decimal) 
            <*> (",a=" *> decimal) 
            <*> (",s=" *> decimal)) 
            <* "}"

inputP :: Parser ([Work], [Part])
inputP = (,) <$> (workFlowP `sepBy1` endOfLine) <*> (skipSpace *> (partP `sepBy1` endOfLine))

-- Part 1. Without ranges
classify :: Classifier -> Part -> Maybe Val
classify (Def v) _ = Just v
classify (Class field cmp n val) part = if cmpf cmp (part ^. getField field) n then Just val else Nothing
    where getField Xtreme = x
          getField Musical = m
          getField Aero = a
          getField Shiny = s

          cmpf Less = (<)
          cmpf Greater = (>)

runWorkflow :: Process -> Part -> Val -> Val
runWorkflow mp part (Other txt) = runWorkflow mp part nextValue
    where (Work _ classis) = mp M.! txt
          nextValue = fromJust $ asum $ (`classify` part) <$> classis
runWorkflow _ _ v = v

part1 :: Process -> [Part] -> Int
part1 workflows parts' = sum $ partSum <$> (filter (doesAccept $ workflows) parts')
    where partSum (Part x m a s) = x + m + a + s
          doesAccept proc part = Accept == runWorkflow proc part (Other "in")

-- Part 2. Now With Ranges TM
--
isValidRange :: Range -> Bool
isValidRange = uncurry (<=)

classifyRange :: Classifier -> PartRange -> (Maybe (Val, PartRange), Maybe PartRange)
classifyRange (Def v) part = (Just (v, part), Nothing)
classifyRange (Class field cmp n val) part = (succPart, remPart)
    where getLens Xtreme = xrange; getLens Musical = mrange; getLens Aero = arange; getLens Shiny = srange

          splitBy Less (lo', hi') n' = both %~ makeSafe $ ((lo', n-1), (n', hi'))
          splitBy Greater (lo', hi') n'  = both %~ makeSafe $ ((n+1, hi'), (lo', n'))
          makeSafe = mfilter (isValidRange) . Just

          succPart = ((val,) . ($ part)) <$> (getLens field .~) <$> succRange
          remPart = ($ part) <$> (getLens field .~) <$> remRange

          (succRange, remRange) = splitBy cmp (part ^. getLens field) n

runWorkflowRange :: Process -> (Val, PartRange) -> [(Val, PartRange)]
runWorkflowRange proc (Other txt, part) = (mapMaybe fst $ scanl go (Nothing, Just part) classis) >>= runWorkflowRange proc
    where go (_, Nothing) _ = (Nothing, Nothing)
          go (_, Just partRange) cls = classifyRange cls partRange
          (Work _ classis) = proc M.! txt
runWorkflowRange _ (v, part) = [(v, part)]

part2 :: Process -> Int
part2 proc = sum $ map (partRangeSum . snd) $ filter ((==Accept) . fst) $ runWorkflowRange proc start
    where partRangeSum (PartRange x m a s) = product $ rangeLen <$> [x,m,a,s]
          rangeLen (lo, hi) = hi - lo + 1
          start = (Other "in", PartRange (1,4000) (1,4000) (1,4000) (1,4000))


