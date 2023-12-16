module Day15 (solveFrom) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.Trans.Reader
import Control.Monad.ST

data Operation = Add T.Text Int | Rem T.Text deriving(Show)
type BoxMap = M.Map T.Text (Int, Int)
data Box = Box BoxMap Int deriving (Show)

type MBoxes s = MV.MVector s Box
type BoxComputation s = ReaderT (MBoxes s) (ST s)

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 split, show $ part2 operations)
    where split = T.split (==',') $ T.init txt
          operations = parseOperation <$> split

hash :: T.Text -> Int
hash = fromIntegral . T.foldl' go 0
    where go n = (*17) . (+n) . toWord8 . fromEnum
          toWord8 = fromIntegral :: Int -> Word8

parseOperation :: T.Text -> Operation
parseOperation txt 
    | T.elem '=' txt = uncurry Add $ read . T.unpack . T.tail <$> T.break (=='=') txt
    | otherwise = Rem $ T.takeWhile (/='-') txt

modifyBox :: Int -> (Box -> Box) -> BoxComputation s ()
modifyBox n f = do
    v <- ask
    MV.modify v f n 

addLens :: T.Text -> Int -> Box -> Box
addLens txt foc (Box mp maxtime) = Box (M.insertWith update txt (foc, maxtime) mp) (maxtime+1)
    where update (focNew,_) (_, tim) = (focNew, tim)

popLens :: T.Text -> Box -> Box
popLens txt (Box mp maxtime) = Box (M.delete txt mp) maxtime

runOperation :: Operation -> BoxComputation s ()
runOperation (Add txt n) = do
    let idx = hash txt
    modifyBox idx $ addLens txt n
runOperation (Rem txt) = do
    let idx = hash txt
    modifyBox idx $ popLens txt

toList :: Box -> [Int]
toList (Box mp _) = map fst $ sortOn snd $ M.elems mp

part1 :: [T.Text] -> Int
part1 = sum . map hash
part2 :: [Operation] -> Int
part2 ops = let timesScore = sum . zipWith (*) [1..] in
    timesScore $ map (timesScore . toList) $ V.toList $ V.create $ do
    newBox <- MV.replicate 256 (Box M.empty 0)
    runReaderT (mapM_ runOperation ops) newBox
    return newBox

