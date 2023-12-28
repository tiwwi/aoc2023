{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Day24 (solveFrom) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as A
import Helpers

import Linear.Metric
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.Vector
import Control.Monad
import Control.Lens

import Data.List(scanl')
import Debug.Trace


data Line = Line {_p :: V3 Double, _d :: V3 Double} deriving (Show)

makeLenses ''Line

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 stones, show $ part2 stones)
    where stones = quickParseT (hailstoneP `A.sepBy1` A.endOfLine) txt

hailstoneP :: A.Parser Line
hailstoneP = do
    [px,py,pz] <- A.sepBy1 (fromIntegral <$> A.signed A.decimal) ("," >> A.skipSpace)
    void $ A.skipSpace >> "@" >> A.skipSpace
    [vx,vy,vz] <- A.sepBy1 (fromIntegral <$> A.signed A.decimal) ("," >> A.skipSpace)
    return $ Line (V3 px py pz) (V3 vx vy vz)
-- p + tv = p' + t'v'
-- tv - t'v' = p'-p

stoneMeet :: Line -> Line -> Maybe (V2 Double)
stoneMeet (Line pfull vfull) (Line pfull' vfull') = do
    let p  = pfull ^. _xy
        v  = vfull ^. _xy
        p' = pfull' ^. _xy
        v' = vfull' ^. _xy
        mat = Linear.Matrix.transpose $ V2 v (-v')
    guard $ det22 mat /= 0
    let (V2 t t') = inv22 mat !* (p' - p)
    guard $ t > 0 && t' > 0
    return $ p + t *^ v

inArea :: V2 Double -> Bool
inArea (V2 x y) = checkComponent x && checkComponent y
    where checkComponent c = 200000000000000 <= c && c <= 400000000000000
inAreaT :: V2 Double -> Bool
inAreaT (V2 x y) = checkComponent x && checkComponent y
    where checkComponent c = 7 <= c && c <= 27


part1 stones = length $ [ pos | pair <- pairs stones, Just pos <- return $ uncurry stoneMeet pair, inArea pos ]
part2 stones = round $ sum result
    where [l1, _, l2 , _, l3] = take 5 stones
          subLine (Line a b) (Line c d) = Line (c-a) (d-b)
          l2'@(Line p2' v2') = subLine l2 l1
          l3'@(Line p3' v3') = subLine l3 l1
          c2' = cross p2' v2'
          c3' = cross p3' v3'
          t2 = negate $ dot p2' c3' / dot v2' c3'
          t3 = negate $ dot p3' c2' / dot v3' c2'
          atTime (Line p v) t = p + t*^v
          result = traceShowId $ atTime l2 t2 + (t2/(t2-t3)) *^ (atTime l3 t3 - atTime l2 t2)
