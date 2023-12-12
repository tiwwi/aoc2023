module Helpers (readT, readMaybeT, decimalToInt, quickParseT, replaceAll) where

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as T
import Data.List (isPrefixOf, find)
import Text.Read (readMaybe)
import Debug.Trace

readT :: Read a => T.Text -> a
readT = read . T.unpack

readMaybeT :: Read a => T.Text -> Maybe a
readMaybeT = readMaybe . T.unpack

decimalToInt :: [Int] -> Int
decimalToInt = foldl go 0
    where go acc x = 10*acc + x

quickParseT :: T.Parser a -> T.Text -> a
quickParseT p txt = case T.parseOnly p txt of
                        Right a -> a
                        Left err -> error err

replaceAll :: (Eq a, Foldable t, Show a) => t ([a], [a]) -> [a] -> [a]
replaceAll _ [] = []
replaceAll patterns xs@(x:re) = maybe (x:replaceAll patterns re) replaceStart replacer
    where replacer = find ((`isPrefixOf` xs) . fst) patterns
          replaceStart (from,to) = to ++ replaceAll patterns (drop (length from) xs)
